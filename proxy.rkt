#lang racket/base

(require typed/web-server/http ;; try to move this to all typed once there is some progress on the rest
         web-server/servlet-dispatch
         web-server/web-server
         "proxy/request.rkt")

(define (proxy req)
  (println req)
  (proxy-request-handler req))

(define (start)
  (serve
   #:dispatch (dispatch/servlet proxy)
   #:port 8888))

(define stop (start))
