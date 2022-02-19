#lang typed/racket

;; these types should probably live with the data storage stuff instead of here
;; the proxy should care mostly about getting the request made and the types it uses
;; are based on the integration with the data store

(provide (struct-out neutral-request-response)
         (struct-out status)
         (struct-out neutral-request)
         (struct-out neutral-response)
         Http-Version
         Status-Code
         Status-Message)

(require typed/net/url-structs
         typed/web-server/http) ;; this file should provide `header` to make the api more sane

;;(define-type Http-Version (U False "HTTP/1.1" "HTTP/2")) ;; not sure how to make the type checker ok with this, but would like it still...
(define-type Http-Version (Option String)) ; should I keep these as Bytes?
(define-type Status-Code (Option Nonnegative-Integer))
(define-type Status-Message (Option String))

#|
keeping these transparent fits in with this being a repl driven tool
the idea is to be able to look at the data by drilling down with functions
so being able to see and drill down interactively is the whole point
once there are multiple workers handling things in the background digging through the requests and responses will be a big task
|#
(struct status ((http-version : Http-Version)
                (code : Status-Code)
                (message : Status-Message)
                (raw : String))
  #:transparent)

(struct neutral-request ((url : url)
                         (method : Bytes)
                         (data : (Option Bytes)) ;; maybe this is a port...
                         (headers : (Listof header)))
  #:transparent)

(struct neutral-response ((status : status)
                          (headers : (Listof header))
                          (body : Bytes))
  #:transparent)

(struct neutral-request-response ((request : neutral-request)
                                  (response : neutral-response))
  #:transparent)

