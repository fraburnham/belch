#lang racket/base

;; This should really be typed. Stuff can change and the type checker
;; won't help us out. If this were typed then the structs would be
;; serializable in http/types.rkt like they should be.

(provide save)

(require racket/serialize
         (prefix-in client: net/url-structs)
         (prefix-in web: web-server/http) ; there was a more specific structs file...
         (prefix-in http: "../http/types.rkt"))


(serializable-struct path/param (path param))
(serializable-struct url (scheme user host port path-absolute? path query fragment))
(serializable-struct header (field value))
(serializable-struct param (name  value))
(serializable-struct status (http-version code message raw))
(serializable-struct request (url method data headers params))
(serializable-struct response (status headers body))
(serializable-struct request-response (request response originator))

(define (http:path/param->path/param p)
  (path/param (client:path/param-path p)
              (client:path/param-param p)))

(define (http:url->url u)
  (url (client:url-scheme u)
       (client:url-user u)
       (client:url-host u)
       (client:url-port u)
       (client:url-path-absolute? u)
       (map http:path/param->path/param (http:url-path u))
       (client:url-query u)
       (client:url-fragment u)))

(define (http:header->header h)
  (header (http:header-field h)
          (http:header-value h)))

(define (http:param->param p)
  (param (http:param-name p)
         (http:param-value p)))

(define (http:status->status s)
  (status (http:status-http-version s)
          (http:status-code s)
          (http:status-message s)
          (http:status-raw s)))

(define (http:request->serializable-request req)
  (request (http:url->url (http:request-url req))
           (http:request-method req)
           (http:request-data req)
           (map http:header->header (http:request-headers req))
           (map http:param->param (http:request-params req))))

(define (http:response->serializable-response resp)
  (response (http:status->status (http:response-status resp))
            (http:response-headers resp)
            (http:response-body resp)))

(define (http:request-response->serializable-request-response req-resp)
  (request-response
   (http:request->serializable-request (http:request-response-request req-resp))
   (http:response->serializable-response (http:request-response-response req-resp))
   (http:request-response-originator req-resp)))

(define (save filepath data)
  (let ((out (open-output-file filepath))
        (data (map http:request-response->serializable-request-response data)))
    (write (serialize data) out)
    (close-output-port out)))
