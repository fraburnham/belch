#lang typed/racket

(provide form-params
         url-paths)

(require "../http/params.rkt"
         "../http/types.rkt")

(: replace (All (a) (-> a a (Listof a) (Listof a))))
(define (replace element new-element elements)
  (map
   (lambda ((e : a))
     (if (equal? element e)
         new-element
         e))
   elements))

(: make-payloads (All (a b) (-> (-> a b b) (Listof b) (Listof a) (Listof (Listof b)))))
(define (make-payloads xfrm elements payloads)
  (remove-duplicates
   (append*
    (map
     (lambda ((element : b)) : (Listof (Listof b))
       (map
        (lambda ((payload : a)) : (Listof b)
          (let ((new-element : b (xfrm payload element)))
            (replace element new-element elements)))
        payloads))
     elements))))

(: url-paths (All (a) (-> (-> a path/param path/param) request (Listof a) (Listof request))))
(define (url-paths xfrm req payloads)
  (let* ((p/p : (Listof path/param) (url-path (request-url req)))
         (payload-p/p : (Listof (Listof path/param)) (make-payloads xfrm p/p payloads)))
    (map
     (lambda ((p/p : (Listof path/param))) : request
       (struct-copy request
                    req
                    (url (struct-copy url
                                      (request-url req)
                                      (path p/p)))))
     payload-p/p)))

(: form-params (All (a) (-> (-> a param param) request (Listof a) (Listof request))))
(define (form-params xfrm req payloads)
  (let* ((params : (Listof param) (request-params req))
         (payload-params : (Listof (Listof param)) (make-payloads xfrm params payloads)))
    (map
     (lambda ((ps : (Listof param))) : request
       (struct-copy request
                    req
                    (headers (filter
                              (lambda ((h : header))
                                (not (string=? (bytes->string/utf-8 (header-field h))
                                               "content-length")))
                              (request-headers req)))
                    (data (params->bytes ps))
                    (params ps)))
     payload-params)))
