#lang racket/base

(require web-server/servlet-dispatch
         web-server/web-server
         (prefix-in proxy: "proxy/core.rkt"))

(provide start)

;; returns a fn to stop the proxy server
; (: start (-> (-> Void)))
(define (start chan)
  (serve
   #:dispatch (dispatch/servlet (proxy:request-handler chan))
   #:port 8888))
