#lang typed/racket

(provide status-line->status)

(require "types.rkt")

#|
Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
|#

(: status-line->status (-> String status))
(define (status-line->status status-line)
  (let ((status-list (string-split status-line " ")))
    (if (>= (length status-list) 3)
        (let* ((http-version (first status-list))
               (http-version : (Option Http-Version) (if (http-version? http-version)
                                                              http-version
                                                              #f))
               (code (string->number (second status-list)))
               (code : (Option Status-Code) (if (and (exact-positive-integer? code) (< code 600))
                                                     code
                                                     #f))
               (message : String (string-join (drop status-list 2) " ")))
          (status http-version code message status-line))
        (status #f #f #f status-line))))
