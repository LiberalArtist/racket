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

;; Tie together enumerations of JS-Expressions, or subsets of them.
(define (knot-json/e knot-object/e knot-array/e atom/e)
  (define-syntax-rule (or/c ? ...)
    (λ (x) (or (? x) ...)))
  (letrec ([expr/e
            (delay/e (or/e (cons array/e list?)
                           #;(cons object/e hash?)
                           (cons atom/e (or/c exact-integer?
                                              inexact-rational?
                                              boolean?
                                              string?
                                              jsnull?))))]
           [array/e (knot-array/e expr/e)]
           [object/e #(knot-object/e expr/e)])
    ;; match the order of our arguments
    (values object/e array/e atom/e expr/e)))
(define ((knot-object/e key/e) expr/e)
  (or/e
   (single/e #hasheq())
   (map/e #:contract (recursive-contract
                      (and/c (hash/c symbol? (enum-contract expr/e) #:immutable #t)
                             hash-eq?
                             (property/c hash-count (not/c 0)))
                      #:flat)
          (match-lambda
            [(cons keys index)
             (for/hasheq ([k (in-list
                              (sort (set->list keys) symbol<?))]
                          [v (in-list
                              (from-nat (listof-n/e expr/e (set-count keys))
                                        index))])
               (values k v))])
          (λ (hsh)
            (cons (for/seteq ([k (in-immutable-hash-keys hsh)])
                    k)
                  (to-nat (listof-n/e expr/e (hash-count hsh))
                          (hash-values hsh 'ordered))))
          (cons/e (except/e (set/e key/e) (set))
                  natural/e))))

(define-values [jsobject/e
                jsarray/e
                jsatom/e
                jsexpr/e]
  (knot-json/e (knot-object/e symbol/e)
               list/e
               (or/e jsnull/e
                     bool/e
                     string/e
                     integer/e
                     inexact-rational/e)))


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
  (let* ([max-len 2 ];5] ; related to https://github.com/racket/racket/issues/4684
         [char-list/e
          #;
          (let loop ([len max-len])
            (if (zero? len)
                (single/e '())
                (let ([tail (loop (sub1 len))])
                  (or/e tail
                        (cons/e constrained-char/e tail)))))
          ;
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
(define constrained-symbol/e
  (map/e string->symbol
         symbol->immutable-string
         constrained-string/e
         #:contract (and/c symbol? (property/c symbol->immutable-string
                                               (enum-contract constrained-string/e)))))
constrained-symbol/e
(define int32/e
  (range/e (- (expt 2 31))
           (- (expt 2 31) 1)))
int32/e
(set/e constrained-symbol/e)
(define-values [constrained-object/e
                constrained-array/e
                constrained-atom/e
                constrained-jsexpr/e]
  (knot-json/e (knot-object/e constrained-symbol/e)
               list/e
               (or/e jsnull/e
                     bool/e
                     constrained-string/e
                     int32/e
                     inexact-rational/e)))
constrained-atom/e
;(enum-count constrained-object/e)
(finite-enum? constrained-jsexpr/e)
(from-nat constrained-jsexpr/e 0)