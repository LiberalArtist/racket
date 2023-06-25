#lang at-exp racket ;/base

(module data racket/base
  (require racket/runtime-path)
  (define-runtime-path indent-test-data/
    "indent-test-data/")
  (provide indent-test-data/))
(require 'data)

(module* cli racket
  ;; In a shell, do `. alias.sh` in this directory to be able to run `indent-test-data-cli`.
  (require (submod "..") racket/cmdline)
  (command-line
   #:program "indent-test-data-cli"
   #:usage-help "" "If given no <option>s or only `--redo-python`, equivalent to:"
   "  $ indent-test-data-cli --validate-all"
   #:help-labels
   "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
   "┃   Actions"
   "┃ First"
   #:multi [("--add-from-nat" "-n") N "Add <N>th test datum from enumeration"
            (--add-from-nat N)]
   #:help-labels
   "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
   "┃ Second"
   #:once-each [("--add-random" "-r") count "Add <count> additional test data, chosen at random"
                (--add-random count)]
   ;; --list ?
   #:help-labels
   "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
   "┃ Third"
   #:once-each [("--validate-all" "-a") ("Validate all previously added test data"
                                         "“Validating” does NOT test the `json` library:"
                                         "It instead tests the INPUT to those tests")
                (--validate-all #t)]
   #:multi [("--validate" "-v") N ("Validate <N>th previously added test datum"
                                   "(If combined with `--validate-all`, has no additional effect)")
            (--validate N)]
   #:help-labels
   "    ──────────────────────────────────────────────"
   #:once-each [("--redo-python" "-p" ) "When validating, replace `python.json` files"
                (--redo-python #t)]
   #:help-labels
   "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
   #:args ()
   (for-each add-from-nat (--add-from-nat))
   (when (--add-random)
     (error "--add-random" (--add-random)))
   (cond
     [(or (--validate-all)
          (and (null? (--validate))
               (null? (--add-from-nat))
               (not (--add-random))))
      (error "--validate-all")]
     [else
      (validate-list #:redo-python? (--redo-python) (--validate))])
   (displayln "The end.")
   (exit 0)))

(require "../../../../racket/collects/json/main.rkt")



























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
             (if (module-declared? enum-mpi 'load)
                 (dynamic-require enum-mpi sym)
                 (error "FATAL ERROR: you need to un-comment" (resolve-module-path-index enum-mpi))))
           (define-syntax id (make-variable-like-transformer #'(get 'id))) ...))
  (define/provide
    test-datum/e compound-jsobject/e portable-indent/e
    in-indent-order-cycle random-element to-nat from-nat))


(require 'dynamic-enum
         'system
         rackunit
         syntax/parse/define)
(provide (all-defined-out))
(void
 test-datum/e compound-jsobject/e portable-indent/e
 in-indent-order-cycle random-element to-nat from-nat)
(define (make-flag-parameter name default [check #f])
  (define (guard arg)
    (define n (string->number arg))
    (unless (check n)
      (raise-arguments-error 'indent-test-data-cli
                             "bad argument to switch"
                             "switch" name
                             "expected" check
                             "given" (unquoted-printing-string arg)))
    (if (null? default)
        (cons n (this))
        n))
  (define this
    (make-parameter default (and check guard) name))
  this)
(define-syntax-parse-rule (define-flag-parameters (~seq (~optional (~and #:multi multi))
                                                        (~or* name:id [name:id check:expr])) ...)
  (begin (define name (make-flag-parameter 'name (~? (and 'multi '()) #f) (~? check))) ...))
(define-flag-parameters
  #:multi [--add-from-nat natural-number/c]
  [--add-random exact-positive-integer?]
  #:multi [--validate natural-number/c]
  --validate-all
  --redo-python)



(define (dir-for-nat n)
  (build-path indent-test-data/ (number->string n)))

(define (add-from-nat which)
  (define datum
    (from-nat test-datum/e which))
  (define dir
    (dir-for-nat which))
  (make-directory dir)
  (call-with-output-file* (build-path dir "datum.rktd")
    (λ (out)
      (pretty-write datum out)))
  (with-output-to-file (build-path dir "node.json")
    (λ ()
      (apply node-write-json datum)))
  (validate which))

(define (validate which #:redo-python? [redo-python? #f])
  (define datum
    (from-nat test-datum/e which))
  (define dir
    (dir-for-nat which))
  (check-pred (λ (x)
                (equal? datum (file->value x)))
              (build-path dir "datum.rktd"))
  (check-pred (λ (x)
                (equal? datum (string->jsexpr (file->string x))))
              (build-path dir "node.json"))
  (when (or redo-python? (not (file-exists? (build-path dir "python.json"))))
    (with-output-to-file (build-path dir "python.json")
      #:exists 'truncate/replace
      (λ ()
        (apply node-write-json datum))))
  (check-pred (λ (x)
                (equal? (file->string (build-path dir "node.json")) (file->string x)))
              (build-path dir "python.json"))
  (displayln "TODO: return value")
  #t)

(define (validate-list nats #:redo-python? [redo-python? #f])
  (define problems
    (filter (λ (which)
              (validate which #:redo-python? redo-python?))
            nats))
  (unless (null? problems)
    (error "validate-list" problems)))
