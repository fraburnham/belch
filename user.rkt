#lang typed/racket

(require (prefix-in xss: "attack/xss.rkt")
         "http/types.rkt"
         typed/racket/async-channel
         (prefix-in data: "data/middleware.rkt")
         (prefix-in data: "data.rkt"))

(require/typed "proxy.rkt"
  (start (-> (Async-Channelof request-response) (Async-Channelof request-response) (-> Void))))

(require/typed "data/save.rkt"
  (save (-> String (Listof request-response) Void)))

(struct system ((worker-stoppers : (Listof (-> Void)))
                (proxy-stopper : (-> Void))))

(: run (-> system))
(define (run)
  (let* ((recorder-chan : (Async-Channelof request-response) (make-async-channel #f))
         (recorder-thread : Thread (thread (recorder-handler recorder-chan)))
         (attacker-chan : (Async-Channelof request-response) (make-async-channel #f))
         (attacker-thread : Thread (thread (attacker-handler attacker-chan))))
    (system
     (list (lambda () (kill-thread recorder-thread))
           (lambda () (kill-thread attacker-thread)))
     (start recorder-chan attacker-chan))))

(: stop (-> system Void))
(define (stop s)
  (for ((stopper (system-worker-stoppers s)))
    (stopper))
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

(: recorder-handler (-> (Async-Channelof request-response) (-> Void)))
(define (recorder-handler chan)
  (: handler (-> Void))
  (define (handler)
    (let* ((req-resp : request-response (async-channel-get chan))
           (req-resp : request-response (process-req-resp req-resp)))
      (data:store req-resp)
      (handler)))
  handler)

(: attacker-handler (-> (Async-Channelof request-response) (-> Void)))
(define (attacker-handler chan)
  (: handler (-> Void))
  (define (handler)
    (let* ((req-resp : request-response (async-channel-get chan))
           (req-resp : request-response (process-req-resp req-resp)))
      ;; This means double processing of the request. Factor that out later...
      (map data:store (xss:attack (request-response-request req-resp)))
      (handler)))
  handler)
