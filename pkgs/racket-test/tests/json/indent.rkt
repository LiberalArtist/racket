#lang at-exp racket ;/base

(module* cli racket
  ;; racket -e- "(require (submod \"indent.rkt\" cli))" <...>
  (require (submod "..") racket/cmdline)
  (define (do-flag f c s)
    (define n (string->number s)) 
    (unless (c n) (error 'indent-test-data-cli "bad flag argument\n  expected: ~e\n  given: ~a" c s))
    (f n)
    (exit 0))
  (command-line
   #:program "indent-test-data-cli"
   #:once-any
   ["--generate-random" how-many "help"
    (do-flag exit exact-positive-integer? how-many)]
   ["--generate-from-nat" natural-number "help"
    (do-flag generate-from-nat natural-number/c natural-number)]
   ["--validate" which "help"
    (do-flag exit natural-number/c which)]
   ["--validate-all" "help"
    (exit)]
   #:args ()
   (error 'indent-test-data-cli "missing required command-line flag")))
(module data racket/base
  (require racket/runtime-path)
  (define-runtime-path indent-test-data/
    "indent-test-data/")
  (provide indent-test-data/))
(require 'data
         "../../../../racket/collects/json/main.rkt")



























;
(module enum racket
  (provide (all-from-out data/enumerate/lib)
           (all-defined-out))
  (require data/enumerate/lib)
  (define jsnull/e
    (single/e 'null #:equal? eq?))
  (define inexact-rational/e
    (except/e flonum/e +inf.0 -inf.0 +nan.0
              #:contract (and/c inexact-real? rational?)))
  (define jsatom/e
    (or/e jsnull/e
          bool/e
          string/e
          integer/e
          inexact-rational/e))
  (define jsexpr/e
    (delay/e (or/e (cons jsarray/e list?)
                   (cons jsobject/e hash?)
                   jsatom/e)))
  (define jsarray/e
    (listof/e jsexpr/e))
  (define jsobject/e
    (or/e
     (single/e #hasheq())
     (map/e #:contract (recursive-contract
                        (and/c (hash/c symbol? (enum-contract jsexpr/e)
                                       #:immutable #t)
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
  (define portable-indent/e ; levels above 10 are not reproducible in JS
    (or/e (single/e #\tab) (except/e (below/e 11) 0)))
  (define indent-order/e
    (let ([nats/e (permutations-of-n/e (enum-count portable-indent/e))])
      (define ((convert-all convert) lst)
        (map (λ (x)
               (convert portable-indent/e x))
             lst))
      (map/e #:contract (and/c
                         (listof (enum-contract portable-indent/e))
                         (property/c length (enum-count portable-indent/e))
                         (flat-named-contract 'no-duplicates?
                                              (λ (lst)
                                                (not (check-duplicates lst eqv?)))))
             (convert-all from-nat)
             (convert-all to-nat)
             nats/e)))
  (define (random-element e)
    (from-nat e (random-index e)))
  (define (in-indent-order-cycle [which (random-index indent-order/e)])
    (in-cycle (from-nat indent-order/e which)))
  (define compound-jsobject/e
    (or/e (cons jsarray/e list?)
          (cons jsobject/e hash?)))
  (define test-datum/e
    (list/e portable-indent/e
            compound-jsobject/e)))
(module system racket
  (provide node-write-json
           python-write-json)
  (require json)
  (define ((make-runner name #:try-first [try-first #f] make-args) indent js)
    (define args
      (make-args
       (jsexpr->string js) ; no easy JS equivalent to sort_keys, so make Racket do it
       (if (eqv? #\tab indent)
           @quote{"\t"}
           (number->string indent))))
    (define (err msg . extra-fields)
      (apply raise-arguments-error
             (string->symbol name)
             msg
             (flatten (list extra-fields
                            "arguments..."
                            (unquoted-printing-string
                             (string-append* (for/list ([v args])
                                               (format "\n   ~e" v))))))))
    (define prog
      (or (for*/or ([find (list (compose1 getenv string-upcase)
                                find-executable-path)]
                    [try (list try-first name)]
                    #:when try)
            (find try))
          (err "command not found"
               (if try-first
                   `("variants tried" ,(list try-first name))
                   null))))
    (define code
      (parameterize ([current-input-port (open-input-string "")])
        (apply system*/exit-code prog args)))
    (unless (zero? code)
      (err "command failed"
           "exit code" code
           name prog)))
  (define node-write-json
    (make-runner
     "node"
     (λ (indent json)
       (list "-e"
             @string-append{
 process.stdout.write(JSON.stringify(@|json|,null,@|indent|))
 }))))
  (define python-write-json
    (make-runner
     "python" #:try-first "python3"
     (λ (indent json)
       (list "-c"
             @string-append-immutable{
 import json
 import sys
 json.dump(json.loads(sys.argv[-1]),
 @""       sys.stdout,
 @""       indent=@|indent|,
 @""       sort_keys=True,
 @""       ensure_ascii=False)
}
             json)))))
(module dynamic-enum racket
  (require racket/runtime-path
           syntax/modresolve
           (for-syntax syntax/transformer))
  (define-runtime-module-path-index here-mpi
    '(submod "."))
  (define enum-mpi
    (module-path-index-join '(submod ".." enum) here-mpi))
  (define-syntax-rule (define/provide id ...)
    (begin (provide id ...)
           (define (get sym)
             (if (module-declared? enum-mpi)
                 (dynamic-require enum-mpi sym)
                 (error "FATAL ERROR: you need to un-comment" (resolve-module-path-index enum-mpi))))
           (define-syntax id (make-variable-like-transformer #'(get 'id))) ...))
  (define/provide
    test-datum/e compound-jsobject/e portable-indent/e
    in-indent-order-cycle random-element to-nat from-nat))


(require 'dynamic-enum 'system)
(provide (all-defined-out))
(begin;void
 test-datum/e compound-jsobject/e portable-indent/e
 in-indent-order-cycle random-element to-nat from-nat)

(define (generate-from-nat which)
  (define datum
    (from-nat test-datum/e which))
  (define dir
    (build-path indent-test-data/ (number->string which)))
  (make-directory dir)
  (call-with-output-file* (build-path dir "datum.rktd")
    (λ (out)
      (pretty-write datum out)))
  (with-output-to-file (build-path dir "node.json")
    (λ ()
      (apply node-write-json datum)))
  (validate which))


(define validate void)


