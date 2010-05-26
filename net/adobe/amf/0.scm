;;;
;;;  AMF0 serializer/deserializer
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
;;  http://opensource.adobe.com/wiki/download/attachments/1114283/amf0_spec_121207.pdf

(define-module net.adobe.amf.0
  (use gauche.charconv)
  (use gauche.parameter)
  (use gauche.uvector)
  (use util.match)
  (use srfi-19)
  (use srfi-42)
  (use srfi-43)
  (use binary.io)
  (use net.adobe.amf.3)

  (export amf0-decode amf0-decode-string
          amf0-encode amf0-encode-string
          <amf0-range-error>
          <amf0-reference-error>
          <amf0-type-error>
          )
  )
(select-module net.adobe.amf.0)

;;
;; Entry point
;;

(define (amf0-decode)
  (parameterize ((default-endian 'big-endian)
                 (object-table (make-hash-table 'eq?)))
    (read-value)))

(define (amf0-encode obj)
  (parameterize ((default-endian 'big-endian)
                 (object-table (make-hash-table 'eq?)))
    (write-value obj)))

(define (amf0-decode-string s)
  (with-input-from-string s amf0-decode))

(define (amf0-encode-string obj)
  (with-output-to-string (cut amf0-encode obj)))

;;
;; Exceptions
;;

(define-condition-type <amf0-range-error> <amf-error> #f)

(define-condition-type <amf0-reference-error> <amf-error> #f)

(define-condition-type <amf0-type-error> <amf-error> #f)

;;
;; Reference tables
;;

(define object-table (make-parameter #f))

;;
;; 1.3.1 String
;;

(define (compstr s)
  (if (string-incomplete? s)
    (or (string-incomplete->complete s) s)
    s))

(define (read-utf8)
  (let1 n (read-u16)
    (compstr (cond-expand
              [(or gauche.ces.utf8 gauche.ces.none) (read-block n)]
              [else (ces-convert (read-block n) 'utf-8)]))))

(define (read-utf8-long)
  (let1 n (read-u32)
    (compstr (cond-expand
              [(or gauche.ces.utf8 gauche.ces.none) (read-block n)]
              [else (ces-convert (read-block n) 'utf-8)]))))

(define (ensure-utf8 s)
  (cond-expand
   [(or gauche.ces.utf8 gauche.ces.none) s]
   [else (ces-convert s (gauche-character-encoding) 'utf-8)]))

(define (write-short-str utf8s)
  (when (>= (string-size utf8s) 65536)
    (error <amf0-range-error> "UTF8 string size exceeds limit:" utf8s))
  (write-u16 (string-size utf8s))
  (display utf8s))

(define (write-long-str utf8s)
  (write-u32 (string-size utf8s))
  (display utf8s))

(define (write-utf8 s) (write-short-str (ensure-utf8 s)))

(define (write-utf8-long s) (write-long-str (ensure-utf8 s)))

(define (write-utf8-empty) (write-u16 0))

;;
;; 2 AMF0 data types
;;

(define-constant .num-datatypes. #x12)

;; Data-type specific readers are invoked after the marker value is read.
;; Data-type specific writers are responsible to emit the marker value.

;; 2.2 number
(define (read-number)
  (let1 v (read-f64)
    (if (and (integer? v) (<= (least-fixnum) v (greatest-fixnum)))
      (inexact->exact v)
      v)))
(define (write-number v) (write-u8 #x00) (write-f64 v))

;; 2.3 boolean
(define (read-boolean) (not (zero? (read-u8))))
(define (write-boolean v) (write-u8 #x01) (write-u8 (if v 0 1)))

;; 2.4 string
(define (read-string) (read-utf8))

(define (write-string v) ; also supports long string
  (let1 utf8s (ensure-utf8 v)
    (cond [(>= (string-size utf8s) 65536)
           (write-u8 #x0c) (write-long-str utf8s)]
          [else
           (write-u8 #x02) (write-short-str utf8s)])))

;; 2.5 object
(define (read-object-properties)
  (let* ((key (read-utf8))
         (value (read-value)))
    (if (eq? value 'object-end)
      '()
      (acons key value (read-object-properties)))))

(define (read-object) `(object . ,(read-object-properties)))

(define (write-object-properties alist)
  (dolist (kv alist)
    (write-utf8 (x->string (car kv)))
    (write-value (cdr kv)))
  (write-utf8-empty)
  (write-object-end-marker))

(define (write-object v) (write-u8 #x03) (write-object-properties (cdr v)))

;; 2.6 movie clip - reserved

;; 2.7 null
(define (read-null) 'null)
(define (write-null v) (write-u8 #x05))

;; 2.8 undefined
(define (read-undefined) (undefined))
(define (write-undefined v) (write-u8 #x06))

;; 2.9 reference
(define (read-reference)
  (let1 num (read-u16)
    (or (hash-table-get (object-table) num #f)
        (error <amf0-reference-error> "stray reference:" num))))
(define (write-reference v)
  (write-u8 #x07)
  (write-u16 v))

(define (check-reference?! v)
  (or (hash-table-get (object-table) v #f)
      (begin (hash-table-put! (object-table) v
                              (hash-table-num-entries (object-table)))
             #f)))

;; 2.10 ECMA array
(define (read-ecma-array)
  (let* ((count (read-u32))
         (elts  (read-object-properties)))
    ;; TODO check count?
    `(array . ,elts)))
(define (write-ecma-array v)
  (write-u8 #x08)
  (write-u32 (length (cdr v)))
  (write-object-properties (cdr v)))

;; 2.11 Object end
;;  'object end' value is consumed by other reades and won't appear
;;  in the final result.
(define (read-object-end) 'object-end)
(define (write-object-end-marker) (write-u8 #x09))

;; 2.12 Strict array
(define (read-strict-array)
  (let1 count (read-u32)
    `(array . ,(vector-of-length-ec count (: i count) (read-value)))))
  
(define (write-strict-array v)
  (write-u8 #x0a)
  (write-u32 (vector-length (cdr v)))
  (do-ec (:vector elt (cdr v)) (write-value elt)))

;; 2.13 Date
(define (read-date)
  (receive (secs msecs) (quotient&remainder (round->exact (read-f64)) 1000)
    (read-u16) ; timezone - unused
    (make-time time-utc (* msecs 1000000) secs)))
(define (write-date v)
  (write-u8 #x0b)
  (write-f64 (+ (*. (time-second v) 1000)
                (round (/. (time-nanosecond) 1000000))))
  (write-u16 0)) ; timezone - unused

;; 2.14 long string
(define (read-long-string) (read-utf8-long))
;; to write, see write-string above

;; 2.15 unsupported
;; 2.16 recordset - reserved

;; 2.17 XML document
(define (read-xml-document) (read-utf8-long))
(define (write-xml-document v) (write-u8 #x0f) (write-utf8-long (cdr v)))

;; 2.18 typed object
(define (read-typed-object)
  (let1 class-name (read-utf8)
    `(typed-object ,class-name ,@(read-object-properties))))

(define (write-typed-object v)
  (write-u8 #x10)
  (write-utf8 (cadr v)) ; class-name
  (write-object-properties (cddr v)))

;; 3.1 AVM+ type marker
(define (read-avm+) (amf3-decode))
(define (write-avm+-marker) (write-u8 #x11))

;;
;; Tie them together
;;

(define-constant .read-dispatch-table0.
  `#(,read-number
     ,read-boolean
     ,read-string
     ,read-object
     ,(cut read-dummy #x04) ;movie clip
     ,read-null
     ,read-undefined
     ,read-reference
     ,read-ecma-array
     ,read-object-end
     ,read-strict-array
     ,read-date
     ,read-long-string
     ,(cut read-dummy #x0d) ;unsupported object
     ,(cut read-dummy #x0e) ;recordset
     ,read-xml-document
     ,read-typed-object
     ,read-avm+))

(define (read-dummy x)
  (error <amf0-type-error> "Unknown or unsupported marker:" x))

(define (read-value)
  (let1 marker (read-u8)
    (when (>= marker .num-datatypes.)
      (error <amf0-type-error> "Unknown type marker:" marker))
    ((vector-ref .read-dispatch-table0. marker))))

(define (write-value v)
  (match v
    [(? real?) (write-number v)]
    [(? string?) (write-string v)]
    [(or #f #t) (write-boolean v)]
    ['null (write-null)]
    [(? time?) (write-date v)]
    [(? undefined?) (write-undefined v)]
    [(= check-reference?! (? identity val)) (write-reference val)]
    [('array . (? vector?)) (write-strict-array v)]
    [('array . (? list?))   (write-ecma-array v)]
    [('object . _) (write-object v)]
    [('xmldocument . _) (write-xml-document v)]
    [_ (error <amf0-type-error> "unserializable object:" v)]))

