#lang typed/racket

(provide follow-redirect
         send)

(require typed/net/http-client
         typed/net/url-structs
         typed/net/url
         (only-in typed/web-server/http
                  headers-assq*)
         "header.rkt"
         "status.rkt"
         "types.rkt")

(: path/params->path-string (-> (Listof path/param) String))
(define (path/params->path-string path/params)
  (foldl
   (lambda ((path/param : path/param)
            (path-accumulator : String))
     (let* ((path (path/param-path path/param))
            (path : String (if (string? path) path "")))
       (string-append path-accumulator "/" path)))
   ""
   path/params))

;; caller closes body-port!
(: read-body (-> Input-Port Bytes Bytes))
(define (read-body body-port output-bytes)
  (let ((this-byte : (U Byte EOF) (read-byte body-port)))
    (if (eof-object? this-byte)
        (bytes->immutable-bytes output-bytes) ; this is kept as bytes to defer thinking about encoding, not for mutability (but the typecheker doesn't check...)
        (read-body body-port
                   (bytes-append output-bytes (bytes this-byte))))))

(: follow-redirect (-> request-response (Listof request-response)))
(define (follow-redirect req-resp)
  (: recur (-> request-response (Listof request-response) (Listof request-response)))
  (define (recur req-resp return-reqs-resps)
    (let* ((req : request (request-response-request req-resp))
           (resp : response (request-response-response req-resp))
           (status-code : (Option Status-Code) (status-code (response-status resp)))
           (status-code : Status-Code (if (false? status-code) 0 status-code)))
      (if (or (= status-code 301) (= status-code 302))
          (let ((location : (Option header) (headers-assq* #"Location" (response-headers resp))))
            (if (header? location)
                (let* ((referrer : header (header #"Referrer" (string->bytes/utf-8 (url->string (request-url req)))))
                       (req-method : Request-Method #"GET")
                       ;; the referrer header isn't being used
                       (req : request (struct-copy request
                                                   req
                                                   (url (string->url (bytes->string/utf-8 (header-value location))))
                                                   (method req-method)))
                       (resp : response (send req)))
                  (recur
                   (request-response req
                                     resp
                                     (request-response-originator req-resp))
                   (cons req-resp return-reqs-resps)))
                ;; failed to find location to follow!
                (cons req-resp return-reqs-resps)))
          (cons req-resp return-reqs-resps))))
  (recur req-resp '()))

(: send (-> request response))
(define (send request)
  (let* ((url : url (request-url request))
         (host (url-host url))
         (host : String (cond ((string? host) host)
                              (else ""))))
    ;; would be nice to be able to pass the url type to an http client...
    ;; will need to do stuff like assemble the path as it stands
    ;; getting a 400 back from servers so this is probably fucked until I fix the path stuff and headers and such
    (let-values ((((status-line : Bytes) (headers : (Listof Bytes)) (body-port : Input-Port))
                  (http-sendrecv host
                                 (path/params->path-string (url-path url))
                                 #:ssl? #f
                                 #:headers (headers->header-bytes (request-headers request))
                                 #:method (request-method request)
                                 #:data (request-data request)
                                 #:content-decode '())))
      (let ((body (read-body body-port #"")))
        (close-input-port body-port)
        (response (status-line->status (bytes->string/utf-8 status-line))
                  (header-bytes->headers headers)
                  body)))))
