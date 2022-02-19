#lang typed/racket

(provide send)

(require typed/net/http-client
         typed/net/url-structs
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
