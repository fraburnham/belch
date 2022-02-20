#lang typed/racket

(provide attack)

#|
 Going super basic to start
 See if the param can be a number. If so generate some nearby numbers.
 Generate new (Listof path/param) for each nearby number.
 Generate new request for each new (Listof path/param).
 Send requests & get responses.
 Build request-responses to return.
|#

(require "../http/types.rkt"
         "../http/request.rkt")

(: positive-integer? (-> Any Boolean : Positive-Integer))
(define positive-integer? (make-predicate Positive-Integer))

(: replace-path-with-nearby (-> (Listof path/param) Number (Listof (Listof path/param))))
(define (replace-path-with-nearby p/p id)
  (let* ((id-str : String (number->string id))
         (min-id : Number (- id 10))
         (min-id : Positive-Integer (if (positive-integer? min-id) min-id 1))
         (max-id : Positive-Integer (if (positive-integer? id) (+ id 10) 10))) 
    (map
     (lambda ((new-id : Number)) : (Listof path/param)
       (map
        (lambda ((existing-p/p : path/param)) : path/param
          (if (equal? (path/param-path existing-p/p) id-str)
              (struct-copy path/param existing-p/p (path (number->string new-id)))
              existing-p/p))
        p/p))
     (range min-id (add1 max-id)))))

(: create-attack-paths (-> (Listof path/param) (Listof (Listof path/param))))
(define (create-attack-paths paths/params)
  (append*
   (map
    (lambda ((p/p : path/param)) : (Listof (Listof path/param))
      (let* ((path (path/param-path p/p))
             (path (if (string? path) (string->number path) #f)))
        (if (number? path)
            (replace-path-with-nearby paths/params path)
            ;; this doesn't feel right, but figure it out (maybe it is right, this path can't be fudged so move on w/ no attacks related to it)
            '())))
    paths/params)))

(: create-attack-requests (-> request (Listof request)))
(define (create-attack-requests req)
  (map
   (lambda ((p : (Listof path/param))) : request
     (struct-copy request req (url (struct-copy url (request-url req) (path p)))))
   (create-attack-paths (url-path (request-url req)))))

(: attack (-> request (Listof request-response)))
(define (attack req)
  (if (get? (request-method req))
      (map
       (lambda ((req : request)) : request-response
         (request-response req
                           (send req)))
       (create-attack-requests req))
      '()))
