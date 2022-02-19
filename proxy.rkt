#lang racket/base

(require typed/web-server/http ;; try to move this to all typed once there is some progress on the rest
         web-server/servlet-dispatch
         web-server/web-server
         "proxy/request.rkt")

(provide start)

;; returns a fn to stop the proxy server
; (: start (-> (-> Void)))
(define (start chan)
  (serve
   #:dispatch (dispatch/servlet (proxy-request-handler chan))
   #:port 8888))
