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

(: store-req-resp (-> request-response Void))
(define (store-req-resp req-resp)
  (let ((middleware (data:middlewares '() (list data:gunzip))))
    (data:store middleware req-resp))
  (void))

(: worker-handler (-> (Async-Channelof request-response) (-> Void)))
(define (worker-handler chan)
  (: handler (-> Void))
  (define (handler)
    (let ((req-resp (async-channel-get chan)))
      (run-auto-attacks req-resp)
      (store-req-resp req-resp)
      (handler)))
  handler)
