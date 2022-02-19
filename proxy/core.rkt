#lang typed/racket

(provide request-handler)

(require typed/racket/async-channel
         typed/web-server/http
         (prefix-in http: "../http/header.rkt")
         (prefix-in http: "../http/request.rkt")
         (prefix-in http: "../http/status.rkt")
         (prefix-in http: "../http/types.rkt"))

(: request->http:request (-> request http:request))
(define (request->http:request request)
  (let* ((method : Bytes (request-method request))
         ;; I wonder if this will cause me headache. The case where an incoming request
         ;; doesn't have a method seems absurd based on the contract provided by the
         ;; web server lib, but it doesn't constrain the method as much as I do...
         (method : http:Request-Method (if (http:request-method? method) method #"GET")))
    (http:request (request-uri request)
                  method
                  (request-post-data/raw request)
                  (request-headers/raw request)
                  '())))

(: http:response->response (-> http:response response))
(define (http:response->response http:response)
  (let* ((status : http:status (http:response-status http:response))
         (status-code : (Option http:Status-Code) (http:status-code status))
         (status-code : Nonnegative-Integer (if (exact-positive-integer? status-code) status-code 599))
         (status-message : (Option http:Status-Message) (http:status-message status))
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
               (http:response-headers http:response))
              (lambda ((out : Output-Port))
                (write-bytes (http:response-body http:response) out)))))

(: request-handler (-> (Async-Channelof http:request-response) (Async-Channelof http:request-response) (-> request response)))
(define ((request-handler recorder-chan attacker-chan) req)
  (let* ((request : http:request (request->http:request req))
         (response : http:response (http:send request)))
    (async-channel-put recorder-chan (http:request-response request response))
    (async-channel-put attacker-chan (http:request-response request response))
    (http:response->response response)))
