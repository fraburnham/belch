#lang typed/racket

(provide proxy-request-handler)

(require typed/net/http-client
         typed/net/url-structs
         typed/racket/async-channel
         typed/web-server/http
         "types.rkt")

(define header-sep-byte 58)

#|
Status-Line = HTTP-Version SP Status-Code SP Reason-Phrase CRLF
|#
(: status-line->status (-> String status))
(define (status-line->status status-line)
  (let ((status-list (string-split status-line " ")))
    (if (>= (length status-list) 3)
        (let* ((http-version (first status-list))
               (http-version : Http-Version (if (or (equal? http-version "HTTP/1.1") (equal? http-version "HTTP/2"))
                                                http-version
                                                #f))
               (code (string->number (second status-list)))
               (code : Status-Code (if (and (exact-positive-integer? code) (< code 600))
                                       code
                                       #f))
               (message : String (string-join (drop status-list 2) " ")))
          (status http-version code message status-line))
        (status #f #f #f status-line))))

(: trim-leading-whitespace (-> (Listof Byte) (Listof Byte)))
(define (trim-leading-whitespace bytes-list)
  (let ((tab : Byte 9)
        (space : Byte 32))
    (cond ((empty? bytes-list)
           bytes-list)
          ((or (equal? (first bytes-list) space) (equal? (first bytes-list) tab))
           (trim-leading-whitespace (drop bytes-list 1)))
          (else bytes-list))))

#|
message-header = field-name ":" ( field-value )
field-name     = token
field-value    = *( field-content | LWS )
field-content  = <the OCTETs making up the field-value
and consisting of either *TEXT or combinations
of token, separators, and quoted-string>
|#
(: header-bytes->headers (-> (Listof Bytes) (Listof header)))
(define (header-bytes->headers header-byte-list)
  (filter-map
   (lambda ((header-bytes : Bytes))
     (let* ((header-bytes-list : (Listof Byte) (bytes->list header-bytes))
            (sep-loc (index-of header-bytes-list header-sep-byte)))
       (if (exact-positive-integer? sep-loc)
           (let-values ((((field-name : (Listof Byte)) (field-value : (Listof Byte))) (split-at header-bytes-list sep-loc)))
             (header (list->bytes field-name)
                     (list->bytes
                      (trim-leading-whitespace
                       (drop field-value 1)))))
           #f)))
   header-byte-list))

(: headers->header-bytes (-> (Listof header) (Listof Bytes)))
(define (headers->header-bytes headers)
  (map
   (lambda ((header : header))
     (let ((header-field : Bytes (header-field header))
           (header-value : Bytes (header-value header)))
       (bytes-append header-field #": " header-value)))
   headers))

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

(: proxy:make-request (-> neutral-request neutral-response))
(define (proxy:make-request request)
  (let* ((url : url (neutral-request-url request))
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
                                 #:headers (headers->header-bytes (neutral-request-headers request))
                                 #:method (neutral-request-method request)
                                 #:data (neutral-request-data request)
                                 #:content-decode '())))
      (let ((body (read-body body-port #"")))
        (close-input-port body-port)
        (neutral-response (status-line->status (bytes->string/utf-8 status-line))
                          (header-bytes->headers headers)
                          body)))))

(: request->neutral-request (-> request neutral-request))
(define (request->neutral-request request)
  (neutral-request (request-uri request)
                   (request-method request)
                   (request-post-data/raw request)
                   (request-headers/raw request)))

;(: neutral-response->response (-> neutral-response response))
(: neutral-response->response (-> neutral-response response))
(define (neutral-response->response neutral-response)
  (let* ((status : status (neutral-response-status neutral-response))
         (status-code : Status-Code (status-code status))
         (status-code : Nonnegative-Integer (if (exact-positive-integer? status-code) status-code 599))
         (status-message : Status-Message (status-message status))
         (status-message : String (if (string? status-message) status-message "Proxy failed to process status message")))
    (response status-code
              (string->bytes/utf-8 status-message)
              (current-seconds)
              #f ; don't set mime type, let Content-Type set it
              (filter
               (lambda ((h : header))
                 ;; this is a hack to work around issues caused by chunked encoding
                 ;; a real fix will require me understanding how chunked encoding works in real life...
                 (not (bytes=? #"Transfer-Encoding" (header-field h))))
               (neutral-response-headers neutral-response))
              (lambda ((out : Output-Port))
                (write-bytes (neutral-response-body neutral-response) out)))))

(: proxy-request-handler (-> (Async-Channelof neutral-request-response) (-> request response)))
(define ((proxy-request-handler chan) req)
  (let* ((request : neutral-request (request->neutral-request req))
         (response : neutral-response (proxy:make-request request)))
    (async-channel-put chan (neutral-request-response request response))
    (neutral-response->response response)))
