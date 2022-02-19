#lang typed/racket

(provide start
         store
         data)

(require "proxy/types.rkt"
         (prefix-in middleware: "data/middleware.rkt"))

(: data* (Listof neutral-request-response))
(define data* '())

(: store (-> neutral-request-response Void))
(define (store req-resp)
  ;; run some normalizing stuff like decoding the gzipped data
  ;; and storing it as bytes
  (set! data* (cons (middleware:apply req-resp) data*)))

(: data (-> (Listof neutral-request-response)))
(define (data) ; dunno if I care to keep this... it does keep data* safe from set! happening elsewhere
  data*)

(: start (-> Void))
(define (start)
  (set! data* '()))
