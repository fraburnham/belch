#lang racket/base

(require typed/web-server/http ;; try to move this to all typed once there is some progress on the rest
         web-server/servlet-dispatch
         web-server/web-server
         "proxy/request.rkt")

(define (start)
  (serve
   #:dispatch (dispatch/servlet proxy-request-handler)
   #:port 8888))

(define stop (start))
