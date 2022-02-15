#lang racket/base

(require web-server/http
         web-server/servlet-dispatch
         web-server/web-server)

(define (proxy req)
  (println req)
  (response/output
   (lambda (out)
     (displayln "Hello proxy!" out))))

(define (start)
  (serve
   #:dispatch (dispatch/servlet proxy)
   #:port 8888))

(define stop (start))
