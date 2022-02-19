#lang typed/racket

(provide start
         store
         data)

(require "data/types.rkt"
         (prefix-in middleware: "data/middleware.rkt"))

(: data* (Listof request-response))
(define data* '())

(: store (-> middleware:middlewares request-response Void))
(define (store mids req-resp)
  ;; run some normalizing stuff like decoding the gzipped data
  ;; and storing it as bytes
  (set! data* (cons (middleware:apply mids req-resp) data*)))

(: data (-> (Listof request-response)))
(define (data) ; dunno if I care to keep this... it does keep data* safe from set! happening elsewhere
  data*)

(: start (-> Void))
(define (start)
  (set! data* '()))
