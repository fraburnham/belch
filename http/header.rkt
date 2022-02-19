#lang typed/racket

(provide header-bytes->headers
         headers->header-bytes)

(require "types.rkt")

#|
message-header = field-name ":" ( field-value )
field-name     = token
field-value    = *( field-content | LWS )
field-content  = <the OCTETs making up the field-value
and consisting of either *TEXT or combinations
of token, separators, and quoted-string>
|#

(define header-sep-byte 58)

(: trim-leading-whitespace (-> (Listof Byte) (Listof Byte)))
(define (trim-leading-whitespace bytes-list)
  (let ((tab : Byte 9)
        (space : Byte 32))
    (cond ((empty? bytes-list)
           bytes-list)
          ((or (equal? (first bytes-list) space) (equal? (first bytes-list) tab))
           (trim-leading-whitespace (drop bytes-list 1)))
          (else bytes-list))))

(: header-bytes->headers (-> (Listof Bytes) (Listof header)))
(define (header-bytes->headers header-byte-list)
  (filter-map
   (lambda ((header-bytes : Bytes))
     (let* ((header-bytes-list : (Listof Byte) (bytes->list header-bytes))
            (sep-loc (index-of header-bytes-list header-sep-byte)))
       (if (exact-positive-integer? sep-loc)
           (let-values ((((field-name : (Listof Byte)) (field-value : (Listof Byte))) (split-at header-bytes-list sep-loc)))
             (header (list->bytes field-name)
                     (list->bytes
                      (trim-leading-whitespace
                       (drop field-value 1)))))
           #f)))
   header-byte-list))

(: headers->header-bytes (-> (Listof header) (Listof Bytes)))
(define (headers->header-bytes headers)
  (map
   (lambda ((header : header))
     (let ((header-field : Bytes (header-field header))
           (header-value : Bytes (header-value header)))
       (bytes-append header-field #": " header-value)))
   headers))
