#lang typed/racket

(provide apply)

(require "../proxy/types.rkt"
         typed/web-server/http)

(require/typed file/gunzip
  (gunzip-through-ports (-> Input-Port Output-Port Void)))

(define-type Request-Middleware (-> neutral-request neutral-request))
(define-type Response-Middleware (-> neutral-response neutral-response))
(define-type Middleware (-> neutral-request-response neutral-request-response))

(: gunzip Response-Middleware)
(define (gunzip resp)
  (let ((encoding-header : (Option header) (headers-assq* #"Content-Encoding" (neutral-response-headers resp))))
    (if (and (header? encoding-header)
             (equal? (header-value encoding-header) #"gzip"))
        (let ((out : Output-Port (open-output-bytes)))
          (with-handlers ((exn:fail? (lambda (_) resp)))
            (gunzip-through-ports (open-input-bytes (neutral-response-body resp)) out)
            (neutral-response (neutral-response-status resp)
                              (neutral-response-headers resp)
                              (get-output-bytes out))))
        resp)))

;; make a fn in here that will run the request and response middlewares
(: apply Middleware)
(define (apply req-resp)
  (let ((req : neutral-request (neutral-request-response-request req-resp))
        (resp : neutral-response (neutral-request-response-response req-resp)))
    ;; keeping this stupid for now
    ;; would like to get a list of middlewares for each side in later...
    (neutral-request-response req (gunzip resp))))
