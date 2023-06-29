#lang racket
(require data/enumerate/lib
         racket/symbol)


(define constrained-char/e
  (let ([base/e (append/e (range/e 48 57) ;[0-9]
                          (range/e 65 90) ;[A-Z]
                          (range/e 97 122))]) ;[a-z]
                         
    (map/e integer->char
           char->integer
           base/e
           #:contract (and/c char? (property/c char->integer
                                               (enum-contract base/e))))))
(define constrained-string/e
  (let* ([max-len 2]
         [char-list/e
          (apply append/e
                 (for/list ([len (in-inclusive-range 0 max-len)])
                   (cons (listof-n/e constrained-char/e len)
                         (λ (lst)
                           (and (list? lst)
                                (= len (length lst)))))))])
    (map/e list->string
           string->list
           char-list/e
           #:contract (and/c string? (property/c string->list
                                                 (and/c (listof (enum-contract constrained-char/e))
                                                        (property/c length (<=/c max-len))))))))

(define unconstrained/e
  (listof/e (or/e string/e (delay/e unconstrained/e))))
(define constrained/e
  (listof/e (or/e constrained-string/e (delay/e constrained/e))))
unconstrained/e
constrained/e


