#lang typed/racket

;; handle converting from a web-server request into one for the http-client

;; what do I want? I need to take a request and give a different request
;; so I need those types first

;; I want to import request struct as a type from web-server/http I think
(require/typed net/url-structs
  [#:struct path/param ([path : (U String 'up 'same)]
                        [param : (Listof String)])]

  [#:struct url ([scheme : (U False #"http")] ;; make this a U of valid schemes
                 [user : (U False String)]
                 [host : (U False String)]
                 [port : (U False Positive-Index)]
                 [path-absolute? : Boolean]
                 [path : (Listof path/param)]
                 [query : (Listof (Pairof Symbol (U False String)))]
                 [fragment : (U False String)])])

(require/typed web-server/http/request-structs
  [#:struct header ([field : Bytes] [value : Bytes])]

  [#:struct request ([method : Bytes] ;; Make this a U of valid methods, eh?
                     [uri : url]
                     [headers/raw : (Listof header)]
                      ;; skipping the binding struct(s) for now, not obvious that I'll care about bindings until I need file transfers
                     [bindings/raw-promise : (Listof Any)]
                     [post-data/raw : (U False Bytes)]
                     [host-ip : String]
                     [host-port : Number]
                     [client-ip : String])])

;; ok now I have types to bring in a request and work on it!
