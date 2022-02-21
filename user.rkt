#lang typed/racket

(require (prefix-in xss: "attack/xss.rkt")
         (prefix-in idor: "attack/idor.rkt")
         (prefix-in sqli: "attack/sqli.rkt")
         "http/types.rkt"
         (prefix-in http: "http/request.rkt")
         typed/racket/async-channel
         (prefix-in data: "data/middleware.rkt")
         (prefix-in data: "data.rkt"))

(require/typed "proxy.rkt"
  (start (-> (Async-Channelof request-response) (-> Void))))

(require/typed "data/save.rkt"
  (save (-> String (Listof request-response) Void)))

(struct system ((worker-stopper : (-> Void))
                (proxy-stopper : (-> Void))))

(: run (-> system))
(define (run)
  (let* ((work-chan : (Async-Channelof request-response) (make-async-channel #f))
         (work-thread : Thread (thread (work-handler work-chan))))
    (system
     (lambda () (kill-thread work-thread))
     (start work-chan))))

(: stop (-> system Void))
(define (stop s)
  ((system-worker-stopper s))
  ((system-proxy-stopper s))
  (void))

(: process-req-resp (-> request-response request-response))
(define (process-req-resp req-resp)
  (let ((middleware (data:middlewares (list data:post-params) (list data:gunzip))))
    (data:process middleware req-resp)))

(: run-attacks (-> request-response (Listof request-response)))
(define (run-attacks req-resp)
  (if (equal? (request-response-originator req-resp) 'proxy)
      (append*
       (map
        (lambda ((attacker : (-> request (Listof request-response)))) : (Listof request-response)
          (append*
           (map http:follow-redirect (attacker (request-response-request req-resp)))))
        (list xss:attack idor:attack sqli:attack)))
      '()))

(: work-handler (-> (Async-Channelof request-response) (-> Any)))
(define (work-handler chan)
  (: handler (-> Any))
  (define (handler)
    (let* ((req-resp : request-response (async-channel-get chan))
           (req-resp : request-response (process-req-resp req-resp)))
      (data:store req-resp)
      (for ((req-resp : request-response (run-attacks req-resp)))
        (async-channel-put chan req-resp))
      (handler)))
  handler)

(struct ctf-flag ((originator : Symbol)
                  (flags : (Listof Bytes)))
  #:transparent)

(: extract-ctf-flag (-> request-response (Option ctf-flag)))
(define (extract-ctf-flag req-resp)
  (let* ((resp : response (request-response-response req-resp))
         (body : Bytes (response-body resp))
         (originator : Symbol (request-response-originator req-resp))
         (match : (Listof Bytes) (regexp-match* #rx#"\\^FLAG\\^.*?\\$FLAG\\$" body)))
    (if (> (length match) 0)
        (ctf-flag originator match)
        #f)))

(: find-ctf-flags (-> (Listof request-response) (Listof ctf-flag)))
(define (find-ctf-flags data)
  (remove-duplicates
   (filter-map extract-ctf-flag data)))

