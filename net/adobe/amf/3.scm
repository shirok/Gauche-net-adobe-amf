;;;
;;;  AMF3 serializer/deserializer
;;;
;;;   Copyright (c) 2008  Shiro Kawai  <shiro@acm.org>
;;;
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   3. Neither the name of the authors nor the names of its contributors
;;;      may be used to endorse or promote products derived from this
;;;      software without specific prior written permission.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

;; Based on Adobe's specification
;;  http://download.macromedia.com/pub/labs/amf/amf3_spec_121207.pdf

(define-module net.adobe.amf.3
  (use gauche.charconv)
  (use gauche.parameter)
  (use gauche.uvector)
  (use util.match)
  (use srfi-19)
  (use srfi-42)
  (use srfi-43)
  (use binary.io)

  (export amf3-decode amf3-decode-string
          amf3-encode amf3-encode-string
          <amf-error>
          <amf3-range-error>
          <amf3-reference-error>
          <amf3-type-error>
          )
  )
(select-module net.adobe.amf.3)


;; Default object mappings:
;;
;; In deserializing:
;;   undefined -> #<undef>
;;   null      -> 'null
;;   false     -> #f
;;   true      -> #t
;;   integer   -> <integer> object
;;   double    -> <real> object
;;   string    -> <string> object
;;   XMLDocument -> ('xmldocument . <string>)
;;   date      -> <time> object
;;   array     -> ('array . <vector>) | ('array (<string> . <value>) ...)
;;   object    ->
;;   XML       -> ('xml . <string>
;;   bytearray -> <u8vector>

;;
;; Entry point
;;

(define (amf3-decode)
  (parameterize ((default-endian 'big-endian)
                 (string-table (make-hash-table 'string=?))
                 (complex-object-table (make-hash-table 'eq?))
                 (object-traits-table (make-hash-table 'equal?)))
    (read-value)))

(define (amf3-encode obj)
  (parameterize ((default-endian 'big-endian)
                 (string-table (make-hash-table 'string=?))
                 (complex-object-table (make-hash-table 'eq?))
                 (object-traits-table (make-hash-table 'equal?)))
    (write-value obj)))

(define (amf3-decode-string s)
  (with-input-from-string s amf3-decode))

(define (amf3-encode-string obj)
  (with-output-to-string (cut amf3-encode obj)))

;;
;; Exceptions
;;

(define-condition-type <amf-error> <error> #f) ; root of AMF errors

(define-condition-type <amf3-range-error> <amf-error> #f)

(define-condition-type <amf3-reference-error> <amf-error> #f)

(define-condition-type <amf3-type-error> <amf-error> #f)

;;
;; 1.3 Basic Rules
;;

;; U8, U16, U32, DOUBLE is read by read-u8, read-u16, read-u32 and read-f64,
;; and written by write-u8, write-u16, write-u32 and write-f64.
;; Endinanness is set to big-endian during reading/writing AMF stream.

;; 29bit variable length integer.  We can't use read-ber-integer since
;; the range is limited.
(define (read-u29)
  (let1 b0 (read-u8)
    (if (< b0 128)
      b0
      (let1 b1 (read-u8)
        (if (< b1 128)
          (+ (* (- b0 128) 256) b1)
          (let1 b2 (read-u8)
            (if (< b2 128)
              (+ (* (- b0 128) 65536) (* (- b1 128) 256) b2)
              (+ (* (- b0 128) 16777216) (* (- b1 128) 65536)
                 (* (- b2 128) 256) (read-u8)))))))))

(define (write-u29 v)
  (when (or (< v 0) (>= v 536870912))
    (error <amf3-range-error> "Integer out of range:" v))
  (write-ber-integer v))

;; utf-8 string
(define (read-utf8-value u29)
  (cond-expand
   [(or gauche.ces.utf8 gauche.ces.none) (read-block (ash u29 -1))]
   [else (ces-convert (read-block (ash u29 -1)) 'utf-8)]))

(define (read-utf8-vr)
  (let1 b (read-u29)
    (if (even? b)
      (get-reference (string-table) b)
      (read-utf8-value b))))

(define (write-utf8-value s)
  (let1 utf8s (cond-expand
               [(or gauche.ces.utf8 gauche.ces.none) s]
               [else (ces-convert s (gauche-character-encoding) 'utf-8)])
    (write-u29 (+ (* (string-size utf8s) 2) 1))
    (display utf8s)))

(define (write-utf8-vr s)
  (cond [(equal? s "") (write-utf8-empty)]; null string can't be a ref
        [(write-reference-if-possible (string-table) s)]
        [else (put-reference! (string-table) s)
              (write-utf8-value s)]))

(define (write-utf8-empty) (write-u29 1))

;;
;; 2.2 Reference tables
;;

(define string-table (make-parameter #f))
(define complex-object-table (make-parameter #f))
(define object-traits-table (make-parameter #f))

;; for reader
(define (get-reference table u29)
  (or (hash-table-get table (ash u29 -1) #f)
      (error <amf3-reference-error> "Stray reference:" (ash u29 -1))))

;; for writer
(define (put-reference! table v)
  (hash-table-put! table v (hash-table-num-entries table)))

(define (write-reference-if-possible table v)
  (cond [(hash-table-get table v #f) => (lambda (id) (write-u29 (* id 2)) #t)]
        [else #f]))

;;
;; 3 AMF3 data types
;;

(define-constant .num-datatypes. #x0d)

;; Data-type specific readers are invoked after the marker value is read.
;; Data-type specific writers are responsible to emit the marker value.

;; 3.2 undefined
(define (read-undefined) (undefined))
(define (write-undefined v) (write-u8 #x00)) ; undefined marker

;; 3.3 null
(define (read-null) 'null)
(define (write-null v) (write-u8 #x01)) ; null marker

;; 3.4 false
(define (read-false) #f)
(define (write-false v) (write-u8 #x02)) ; false marker

;; 3.5 true
(define (read-true) #f)
(define (write-true v) (write-u8 #x03)) ; true marker

;; 3.6 integer
(define (read-integer) (read-u29))
(define (write-integer v) (write-u8 #x04) (write-u29 v))

;; 3.7 double
(define (read-double) (read-f64))
(define (write-double v) (write-u8 #x05) (write-f64 v))

;; 3.8 string
(define (read-string) (read-utf8-vr))
(define (write-string v) (write-u8 #x06) (write-utf8-vr v))

;; 3.9 XMLDocument
(define (read-xml-common type)
  (let1 b (read-u29)
    (cons 'type
          (if (even? b)
            (get-reference (complex-object-table) b)
            (read-utf8-value b)))))
(define (write-xml-common marker v)
  (write-u8 marker)
  (or (write-reference-if-possible (complex-object-table) v)
      (begin (put-reference! (complex-object-table) v)
             (write-utf8-value (cdr v)))))

(define (read-xml-document) (read-xml-common 'xmldocument))
(define (write-xml-document v) (write-xml-common #x07 v))

;; 3.10 Date
(define (read-date)
  (let1 b (read-u29)
    (if (even? b)
      (get-reference (complex-object-table) b)
      (receive (secs msecs) (quotient&remainder (round->exact (read-f64)) 1000)
        (make-time time-utc (* msecs 1000000) secs)))))
(define (write-date v)
  (write-u8 #x08)
  (or (write-reference-if-possible (complex-object-table) v)
      (begin (put-reference! (complex-object-table) v)
             (write-u8 #x01)
             (write-f64 (+ (*. (time-second v) 1000)
                           (round (/. (time-nanosecond) 1000000)))))))

;; 3.11 Array type
(define (read-array)
  (define (read-assoc-part)
    (match (read-utf8-vr)
      [""  '()]
      [key (let1 v (read-value) (acons key v (read-assoc-part)))]))
  (define (read-dense-part count)
    (list-ec (: i count) (read-value)))
  (let1 b (read-u29)
    (if (even? b)
      (get-reference (complex-object-table) b)
      (let* ((a-part (read-assoc-part))
             (d-part (read-dense-part (ash b -1))))
        (cond [(null? a-part) `(array . ,(list->vector d-part))]
              [(null? d-part) `(array . ,a-part)]
              [else `(array . ,(append (list-ec (: v (index i) d-part)
                                                (cons (x->string i) v))
                                       a-part))])
        ))))
(define (write-array v)
  (write-u8 #x09)
  (cond [(write-reference-if-possible (complex-object-table) v)]
        [(vector? (cdr v))
         (write-u29 (+ (* (vector-length (cdr v)) 2) 1))
         (do-ec (: e (cdr v)) (write-value e))]
        [else
         (write-u29 1)
         (do-ec (: e (cdr v))
                (begin (write-utf8-vr (car e)) (write-value (cdr e))))
         (write-utf8-empty)]))

;; 3.12 Object type
(define (read-object) (error "not supported yet"))
(define (write-object v) (error "not supported yet"))

;; 3.13 XML type
(define (read-xml) (read-xml-common 'xml))
(define (write-xml v) (write-xml-common #x0b v))

;; 3.14 ByteArray type
(define (read-bytearray)
  (let1 b (read-u29)
    (if (even? b)
      (get-reference (complex-object-table) b)
      (rlet1 buf (make-u8vector (ash b -1))
        (read-block! buf)))))
(define (write-bytearray v)
  (write-u8 #x0c)
  (or (write-reference-if-possible (complex-object-table) v)
      (begin (put-reference! (complex-object-table) v)
             (write-u29 (+ (* (u8vector-length v) 2) 1))
             (write-block v))))

;;
;; Tie them together
;;

(define-constant .read-dispatch-table3.
  `#(,read-undefined
     ,read-null
     ,read-false
     ,read-true
     ,read-integer
     ,read-double
     ,read-string
     ,read-xml-document
     ,read-date
     ,read-array
     ,read-object
     ,read-xml
     ,read-bytearray))

(define (read-value)
  (let1 marker (read-u8)
    (when (>= marker .num-datatypes.)
      (error <amf3-type-error> "Unknown type marker:" marker))
    ((vector-ref .read-dispatch-table3. marker))))

(define (write-value v)
  (match v
    [(? real?)
     (if (and (exact? v) (integer? v) (>= v 0) (< v 536870912))
       (write-integer v)
       (write-double v))]
    [(? string?) (write-string v)]
    [#f (write-false)]
    [#t (write-true)]
    ['null (write-null)]
    [(? time?) (write-date v)]
    [('array . _) (write-array v)]
    [('object . _) (write-object v)]
    [('xml . _) (write-xml v)]
    [('xmldocument . _) (write-xml-document v)]
    [(? undefined?) (write-undefined v)]
    [_ (error <amf3-type-error> "unserializable object:" v)]))


