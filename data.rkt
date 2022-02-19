#lang typed/racket

(provide start
         store
         process
         data)

(require "data/types.rkt"
         (prefix-in middleware: "data/middleware.rkt"))

(: data* (Listof request-response))
(define data* '())

(: store (-> request-response Void))
(define (store req-resp)
  ;; run some normalizing stuff like decoding the gzipped data
  ;; and storing it as bytes
  (set! data* (cons req-resp data*)))

(: process (-> middleware:middlewares request-response request-response))
(define (process mids req-resp)
  (middleware:apply mids req-resp))

(: data (-> (Listof request-response)))
(define (data) ; dunno if I care to keep this... it does keep data* safe from set! happening elsewhere
  data*)

(: start (-> Void))
(define (start)
  (set! data* '()))
