#lang typed/racket

(provide apply
         gunzip
         (struct-out middlewares)
         post-params
         Response-Middleware
         Request-Middleware)

(require "../http/types.rkt"
         "params.rkt"
         (prefix-in http: typed/web-server/http))

(require/typed file/gunzip
  (gunzip-through-ports (-> Input-Port Output-Port Void)))

(define-type Request-Middleware (-> request request))
(define-type Response-Middleware (-> response response))

(struct middlewares ((request : (Listof Request-Middleware))
                     (response : (Listof Response-Middleware))))

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

(: post-params Request-Middleware)
(define (post-params req)
  (request (request-url req)
           (request-method req)
           (request-data req)
           (request-headers req)
           (let ((data : (Option Bytes) (request-data req)))
             (if (bytes? data) (post-body->params data) '()))))

(: apply (-> middlewares request-response request-response))
(define (apply mids req-resp)
  (let ((req : request (request-response-request req-resp))
        (resp : response (request-response-response req-resp)))
    (request-response
     (foldl
      (lambda ((request-handler : Request-Middleware) (req : request))
        (request-handler req))
      req
      (middlewares-request mids))
     (foldl
      (lambda ((response-handler : Response-Middleware) (resp : response))
        (response-handler resp))
      resp
      (middlewares-response mids)))))
