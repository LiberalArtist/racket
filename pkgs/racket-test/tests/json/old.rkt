#lang racket
(require data/enumerate/lib
         racket/symbol)
(define (jsnull? x)
  (eq? x 'null))
(define jsnull/e
  (single/e 'null #:equal? eq?))
(define (inexact-rational? x) ; not nan or inf
  (and (inexact-real? x) (rational? x)))
(define inexact-rational/e
  (except/e flonum/e +inf.0 -inf.0 +nan.0 #:contract inexact-rational?))
(define jsatom/e
  (or/e jsnull/e
        bool/e
        string/e
        integer/e
        inexact-rational/e))
(define jsexpr/e
  (delay/e (or/e (cons jsarray/e list?)
                 (cons jsobject/e hash?)
                 (cons jsatom/e (λ (x)
                                  (or (exact-integer? x)
                                      (inexact-rational? x)
                                      (boolean? x)
                                      (string? x)
                                      (jsnull? x)))))))
(define jsarray/e
  (listof/e jsexpr/e))
(define jsobject/e
  (or/e
   (single/e #hasheq())
   (map/e #:contract (recursive-contract
                      (and/c (hash/c symbol? (enum-contract jsexpr/e) #:immutable #t)
                             hash-eq?
                             (property/c hash-count (not/c 0)))
                      #:flat)
          (match-lambda
            [(cons keys index)
             (for/hasheq ([k (in-list
                              (sort (set->list keys) symbol<?))]
                          [v (in-list
                              (from-nat (listof-n/e jsexpr/e (set-count keys))
                                        index))])
               (values k v))])
          (λ (hsh)
            (cons (for/seteq ([k (in-immutable-hash-keys hsh)])
                    k)
                  (to-nat (listof-n/e jsexpr/e (hash-count hsh))
                          (hash-values hsh 'ordered))))
          (cons/e (except/e (set/e symbol/e) (set))
                  natural/e))))
(to-nat jsobject/e
        '#hasheq([EcMKjU0jtXXPbOTyFnr1
                  .
                  (#hasheq([5VBug10xH18yNJL1 . (-4.125454841504056e-309
                                                "CU6gcISfFTIarkqRhLa5OKyeDZp03VmyWbL0"
                                                #hasheq())]
                           [XYnACmDWy41EUCKkauEQLcNIoCK4SGKPJMYikDrxwm3Eg4SI4
                            . "wFTI23c1IOrwZPKeQ0sq8c8rdYsMywDMOPUorCb"]
                           [lzyijzAF6gQczgUzgfAIkqc5OqWaSqDjOgK5hGwnXbIO37xMHU4H41
                            . "CrY7EZyCRutuIPPW5tPFb61kHJYXm3u43xihZKhXP3dfM6"]))]
                 [VpCYZVfWvsipb4aLLDgBcpYcq9wbV8U26NE
                  . "tPGRhTdCbluoyvBQy2BMjdAxB4XdeyNi56"]
                 [cJwHpce5IaTE7ZXdMF . #hasheq()]))
