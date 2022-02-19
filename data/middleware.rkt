#lang typed/racket

(provide apply)

(require "types.rkt"
         "params.rkt"
         (prefix-in http: typed/web-server/http))

(require/typed file/gunzip
  (gunzip-through-ports (-> Input-Port Output-Port Void)))

(define-type Request-Middleware (-> request request))
(define-type Response-Middleware (-> response response))
(define-type Middleware (-> request-response request-response))

(: gunzip Response-Middleware)
(define (gunzip resp)
  (let ((encoding-header : (Option http:header) (http:headers-assq* #"Content-Encoding" (response-headers resp))))
    (if (and (http:header? encoding-header)
             (equal? (http:header-value encoding-header) #"gzip"))
        (let ((out : Output-Port (open-output-bytes)))
          (with-handlers ((exn:fail? (lambda (_) resp)))
            (gunzip-through-ports (open-input-bytes (response-body resp)) out)
            (response (response-status resp)
                      (response-headers resp)
                      (get-output-bytes out))))
        resp)))

(: apply Middleware)
(define (apply req-resp)
  (let ((req : request (request-response-request req-resp))
        (resp : response (request-response-response req-resp)))
    ;; keeping this stupid for now
    ;; would like to get a list of middlewares for each side in later...
    ;; this middleware point here could also be where we tell listeners that a new item has been added...
    ;; or maybe we don't bother and let user.rkt handle that for now...
    (request-response req (gunzip resp))))
