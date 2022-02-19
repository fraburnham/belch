#lang typed/racket

(require typed/racket/async-channel
         "proxy/request.rkt"
         "proxy/types.rkt"
         (prefix-in data: "data.rkt"))

(require/typed "proxy.rkt"
  (start (-> (Async-Channelof neutral-request-response) (-> Void))))

(struct system ((worker-stopper : (-> Void))
                (proxy-stopper : (-> Void))))

(: run (-> system))
(define (run)
  (let* ((chan : (Async-Channelof neutral-request-response) (make-async-channel #f))
         (worker-thread : Thread (thread (worker-handler chan))))
    (system
     (lambda () (kill-thread worker-thread))
     (start chan))))

(: stop (-> system Void))
(define (stop s)
  ((system-worker-stopper s))
  ((system-proxy-stopper s))
  (void))

(: run-auto-attacks (-> neutral-request-response Void))
(define (run-auto-attacks req-resp)
  ;; run automatic attacks like detecting inputs and attempting xss injection
  (void))

(: store-req-resp (-> neutral-request-response Void))
(define (store-req-resp req-resp)
  ;; this would put the req-resp in state somewhere that it can be looked over manually (or automatically)
  ;; if this is abstracted well then storing something could trigger reader workers that check for results

  ;; this fn should have some middleware that unzips the request?
  ;; maybe store itself should
  (data:store req-resp)
  (void))

(: worker-handler (-> (Async-Channelof neutral-request-response) (-> Void)))
(define (worker-handler chan)
  (letrec ((work-handler* : (-> Void)
                          (lambda ()
                            (let ((req-resp (async-channel-get chan)))
                              (run-auto-attacks req-resp)
                              (store-req-resp req-resp)
                              (work-handler*)))))
    work-handler*))
