#lang racket

(require net/http-client)

;; (http-sendrecv "google.com" "/" #:ssl? #t)

;; so do try to make the proxy again
;; all the workers and the data will need to be typed!
;; how will I handle knowing that the data has changed?
;; Maybe a callback of some kind that calls all the workers that handle new data...
;; This would mean less data for them to look at each time, too since they could get only the new request and then could store that data somewhere local to themselves
;; also don't forget racket has greenthreads so somewhere there is a way to handle this like in clj (though I may need a real thread w/ greeens on it...)


