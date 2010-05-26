;;;
;;;  Adobe's Action Message Format (AMF) serializer/deserializer
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

(define-module net.adobe.amf
  (use gauche.parameter)
  (use binary.io)
  (use srfi-42)
  (extend net.adobe.amf.0 net.adobe.amf.3)
  (export read-amf-packet
          write-amf-packet)
  )
(select-module net.adobe.amf)

;;
;; AMF packet parser
;;

(define (read-amf-packet)
  (parameterize ((default-endian 'big-endian))
    (let* ((version (read-u16))
           (header-count (read-u16))
           (header-types (list-ec (: i header-count) (read-amf-header)))
           (message-count (read-u16))
           (message-types (list-ec (: i message-count) (read-amf-message))))
      `(:version ,version
        :headers ,header-types
        :messages ,message-types))))

(define (read-amf-header)
  (let* ((name (read-utf8))
         (must-understand (read-u8))
         (length (read-u32)))
    `(:name ,name
      :must-understand ,(not (zero? must-understand))
      :value ,(amf0-decode))))

(define (read-amf-message)
  (let* ((target-uri (read-utf8))
         (response-uri (read-utf8))
         (length (read-u32)))
    `(:target-uri ,target-uri
      :response-uri ,response-uri
      :value ,(amf0-decode))))
         
;;
;; AMF packet writer
;;

(define (write-amf-packet packet :key (amf-encoding 'amf3) (use-length #t))
  (parameterize ((default-endian 'big-endian))
    (let ((version (get-keyword :version packet 3))
          (headers (get-keyword :headers packet '()))
          (messages (get-keyword :messages packet '())))
      (write-u16 version)
      (write-u16 (length headers))
      (do-ec (: h headers) (write-amf-header h use-length amf-encoding))
      (write-u16 (length messages))
      (do-ec (: m messages) (write-amf-message m use-length amf-encoding))
      )))

(define (write-amf-header h use-length amf-encoding)
  (write-utf8 (get-keyword :name h ""))
  (write-u8 (if (get-keyword :must-understand h #f) 1 0))
  (if use-length
    (let1 body (with-output-to-string
                 (cut write-amf-value (get-keyword :value h "") amf-encoding))
      (write-u32 (string-size body))
      (display body))
    (begin
      (write-u32 #xffffffff)
      (write-amf-value (get-keyword :value h "") amf-encoding))))

(define (write-amf-message m use-length amf-encoding)
  (write-utf8 (get-keyword :target-uri m ""))
  (write-utf8 (get-keyword :response-uri m ""))
  (if use-length
    (let1 body (with-output-to-string
                 (cut write-amf-value (get-keyword :value m "") amf-encoding))
      (write-u32 (string-size body))
      (display body))
    (begin
      (write-u32 #xffffffff)
      (write-amf-value (get-keyword :value m "") amf-encoding))))
      
(define (write-amf-value v amf-encoding)
  (case amf-encoding
    [(amf0) (amf0-encode v)]
    [(amf3) (write-avm+-marker) (amf3-encode v)]
    [else (error "invalid amf-encoding value:" amf-encoding)]))

