#lang typed/racket

(provide param->bytes
         params->bytes)

(require "types.rkt")

(: param->bytes (-> param Bytes))
(define (param->bytes p)
  (bytes-append (param-name p)
                #"="
                (param-value p)))

(: params->bytes (-> (Listof param) Bytes))
(define (params->bytes ps)
  (bytes-join (map param->bytes ps) #"&"))

