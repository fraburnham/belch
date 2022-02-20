#lang typed/racket

(require (prefix-in xss: "attack/xss.rkt")
         (prefix-in idor: "attack/idor.rkt")
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
      ;; I think I can factor this out by attaching info about who generated the req-resp
      ;; so workers can skip it on their own (or select it)
      (map data:store (append*
                       ;; there is a bug in here that the data processers aren't running on these!
                       ;; the requests responses should be popped back on the chan so that the recorder can
                       ;; handle them at the top and the callers can self identify and skip (or otherwise filter)
                       (map
                        (lambda ((attacker : (-> request (Listof request-response)))) : (Listof request-response)
                          (attacker (request-response-request req-resp)))
                        (list xss:attack idor:attack))))
      (handler)))
  handler)

(: find-ctf-flags (-> (Listof request-response) (Listof Bytes)))
(define (find-ctf-flags data)
  (remove-duplicates
   (append*
    (map
     (compose
      (lambda ((body : Bytes)) (regexp-match* #rx#"\\^FLAG\\^.*?\\$FLAG\\$" body))
      (compose response-body
               request-response-response))
     data))))

;; since there is no redirect follower I'm losing some of the data from the edit requests (since it gets clobbered by another edit before it is read)
;; so let's get redirect following as an option for requests (that'll get me all 4 automatically from the microcms v1 ctf)
