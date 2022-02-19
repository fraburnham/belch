#lang typed/racket

(require typed/racket/async-channel
         "proxy/request.rkt"
         "data/types.rkt"
         (prefix-in data: "data/middleware.rkt")
         (prefix-in data: "data.rkt"))

(require/typed "proxy.rkt"
  (start (-> (Async-Channelof request-response) (-> Void))))

(struct system ((worker-stopper : (-> Void))
                (proxy-stopper : (-> Void))))

(: run (-> system))
(define (run)
  (let* ((chan : (Async-Channelof request-response) (make-async-channel #f))
         (worker-thread : Thread (thread (worker-handler chan))))
    (system
     (lambda () (kill-thread worker-thread))
     (start chan))))

(: stop (-> system Void))
(define (stop s)
  ((system-worker-stopper s))
  ((system-proxy-stopper s))
  (void))

(: run-auto-attacks (-> request-response Void))
(define (run-auto-attacks req-resp)
  ;; run automatic attacks like detecting inputs and attempting xss injection
  (void))

(: process-req-resp (-> request-response request-response))
(define (process-req-resp req-resp)
  (let ((middleware (data:middlewares (list data:post-params) (list data:gunzip))))
    (data:process middleware req-resp)))

(: worker-handler (-> (Async-Channelof request-response) (-> Void)))
(define (worker-handler chan)
  (: handler (-> Void))
  (define (handler)
    (let* ((req-resp : request-response (async-channel-get chan))
           (req-resp : request-response (process-req-resp req-resp)))
      (data:store req-resp)
      (run-auto-attacks req-resp)
      (handler)))
  handler)
