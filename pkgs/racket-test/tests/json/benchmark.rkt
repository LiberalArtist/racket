#lang racket/base

(require json
         racket/port
         data/enumerate
         data/enumerate/lib)

(define js-obj
  (for/hasheq ([i (in-range 10000)]
               [k (in-enum symbol/e)])
    (values k #t)))

(define no-out
  (open-output-nowhere))

(define (run-many sort-keys?)
  (printf "sort-keys? = ~v\n" sort-keys?)
  (collect-garbage)
  (collect-garbage)
  (collect-garbage)
  (time
   (for ([i (in-range 10000)])
     (write-json js-obj no-out
                 #:sort-keys? sort-keys?))))

(run-many #t)
(run-many #f)
