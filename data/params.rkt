#lang typed/racket

(provide (struct-out param)
         post-body->params)

;; handle splitting post params
;; in a diff file can handle urlencode/decode (but I don't really care about it yet)

(struct param ((name : Bytes)
               (value : Bytes))
  #:transparent)

(: split-kvp (-> (Listof Byte) (Option param)))
(define (split-kvp kvp)
  (let* ((sep : Byte 61)
         (sep-loc : (Option Index) (index-of kvp sep)))
    (if (index? sep-loc)
        (let-values ((((k : (Listof Byte)) (v : (Listof Byte))) (split-at kvp sep-loc)))
          (param (list->bytes k) (list->bytes (drop v 1))))
        #f)))

(: isolate-kvp (-> (Listof Byte) (Listof param) (Listof param)))
(define (isolate-kvp body params)
  (let* ((kvp-sep : Byte 38)
         (sep-loc : (Option Index) (index-of body kvp-sep)))
    (if (index? sep-loc)
        (let-values ((((kvp : (Listof Byte)) (rest-body : (Listof Byte))) (split-at body sep-loc)))
          (let ((p : (Option param) (split-kvp kvp)))
            (if (param? p)
                  (isolate-kvp (drop rest-body 1) (cons p params))
                  (isolate-kvp (drop rest-body 1) (cons (param #"FAILED TO PARSE" #"FAILED TO PARSE") params)))))
        params)))

(: post-body->params (-> Bytes (Listof param)))
(define (post-body->params body)
  (isolate-kvp (bytes->list body) '()))

