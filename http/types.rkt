#lang typed/racket

(provide (struct-out request-response)
         (struct-out status)
         (struct-out request)
         (struct-out response)
         (struct-out param)
         (struct-out header)
         (struct-out path/param)
         (struct-out url)
         Http-Version
         Request-Method
         Status-Code
         Status-Message
         http-version?
         http/1.1?
         http/2?
         request-method?
         get?
         post?
         head?
         options?
         put?
         delete?)

(require typed/net/url-structs
         typed/web-server/http) ;; this file should provide `header` to make the api more sane

(define-type Http-Version (U "HTTP/1.1" "HTTP/2"))
(define-type Request-Method (U #"GET" #"POST" #"HEAD" #"OPTIONS" #"PUT" #"DELETE"))
(define-type Status-Code Nonnegative-Integer)
(define-type Status-Message String)

(: http-version? (-> Any Boolean : Http-Version))
(define http-version? (make-predicate Http-Version))

(: http/1.1? (-> Any Boolean : "HTTP/1.1"))
(define http/1.1? (make-predicate "HTTP/1.1"))

(: http/2? (-> Any Boolean : "HTTP/2"))
(define http/2? (make-predicate "HTTP/2"))

;; This is a lot of boilerplate. I wonder if a macro could clean it up?
(: request-method? (-> Any Boolean : Request-Method))
(define request-method? (make-predicate Request-Method))

(: get? (-> Any Boolean : #"GET"))
(define get? (make-predicate #"GET"))

(: post? (-> Any Boolean : #"POST"))
(define post? (make-predicate #"POST"))

(: head? (-> Any Boolean : #"HEAD"))
(define head? (make-predicate #"HEAD"))

(: options? (-> Any Boolean : #"OPTIONS"))
(define options? (make-predicate #"OPTIONS"))

(: put? (-> Any Boolean : #"PUT"))
(define put? (make-predicate #"PUT"))

(: delete? (-> Any Boolean : #"DELETE"))
(define delete? (make-predicate #"DELETE"))

#|
keeping these transparent fits in with this being a repl driven tool
the idea is to be able to look at the data by drilling down with functions
so being able to see and drill down interactively is the whole point
once there are multiple workers handling things in the background digging through the requests and responses will be a big task
|#
(struct param ((name : Bytes)
               (value : Bytes))
  #:transparent)

(struct status ((http-version : (Option Http-Version))
                (code : (Option Status-Code))
                (message : (Option Status-Message))
                (raw : String))
  #:transparent)

(struct request ((url : url)
                 (method : Request-Method)
                 (data : (Option Bytes))
                 (headers : (Listof header))
                 (params : (Listof param)))
  #:transparent)

(struct response ((status : status)
                  (headers : (Listof header))
                  (body : Bytes))
  #:transparent)

(struct request-response ((request : request)
                          (response : response))
  #:transparent)
