#lang at-exp racket ;/base

;; This files assumes `(eq? 'null (json-null))`.

(module data racket/base
  (require racket/runtime-path)
  (define-runtime-path indent-test-data/
    "indent-test-data/")
  (provide (all-defined-out)))
(require 'data
         "../../../../racket/collects/json/main.rkt")

;;
;;    WRITE TESTS HERE
;;

(module* cli racket
  ;; In a shell, source `alias.sh` in this directory to be able to run `indent-test-data-cli`.
  (module* main #f
    (require racket/cmdline)
    (command-line
     #:program "indent-test-data-cli"
     #:usage-help "" "If given no <option>s or only `--redo-python`, equivalent to:"
     "  $ indent-test-data-cli --validate-all [ --redo-python ]"
     #:help-labels
     "┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     "┃   Actions"
     "┃ First"
     #:multi [("--add-from-nat" "-n") N "Add <N>th test datum from enumeration."
              (--add-from-nat N)]
     #:help-labels
     "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     "┃ Second"
     #:once-each [("--add-random" "-r") count "Add <count> additional test data, chosen at random."
                  (--add-random count)]
     #:help-labels
     "┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     "┃ Third"
     #:once-each [("--validate-all" "-a") ("Validate all previously added test data."
                                           "“Validating” does NOT test the `json` library:"
                                           "It instead tests the INPUT to those tests.")
                  (--validate-all #t)]
     #:multi [("--validate" "-v")
              N ("Validate <N>th previously added test datum."
                 "(If combined with `--validate-all`, has no additional effect.)"
                 "Data added by `--add-from-nat` and `--add-random` are always validated.")
              (--validate N)]
     #:help-labels
     "    ──────────────────────────────────────────────"
     #:once-each [("--redo-python" "-p" ) "When validating, replace `python.json` files."
                  (--redo-python #t)]
     #:help-labels
     "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
     #:args ()
     (indent-test-data-cli)))

  (provide indent-test-data-cli
           --add-from-nat
           --add-random
           --validate-all
           --validate
           --redo-python)

  ;
  ;
  ;
  ;
  ;
  ;                                                        ;;
  ;   ;;;;                                                 ;;
  ;    ;;             ;                                    ;;
  ;    ;;             ;                                    ;;
  ;    ;;   ;; ;;;  ;;;;;   ;;;;    ;; ;  ;; ;;;    ;;;;   ;;   ;;;;
  ;    ;;   ;;;  ;    ;    ;;   ;   ;;;   ;;;  ;   ;;  ;;  ;;  ;   ;
  ;    ;;   ;;   ;;   ;    ;    ;;  ;;    ;;   ;;       ;  ;;  ;
  ;    ;;   ;;   ;;   ;    ;    ;;  ;;    ;;   ;;       ;  ;;  ;;
  ;    ;;   ;;   ;;   ;    ;;;;;;;  ;;    ;;   ;;   ;;;;;  ;;   ;;;;
  ;    ;;   ;;   ;;   ;    ;        ;;    ;;   ;;  ;    ;  ;;      ;;
  ;    ;;   ;;   ;;   ;    ;        ;;    ;;   ;;  ;    ;  ;;      ;;
  ;    ;;   ;;   ;;   ;    ;;   ;   ;;    ;;   ;;  ;  ;;;  ;;  ;   ;;
  ;   ;;;;  ;;   ;;   ;;;   ;;;;    ;;    ;;   ;;  ;;;; ;; ;;;  ;;;;
  ;
  ;
  ;
  ;
  ;


  (require (for-syntax racket/syntax)
           (submod ".." dynamic-enum)
           (submod ".." #;generate-and-validate)
           syntax/parse/define)
  


  (define not-string/c
    (let ()
      (define (not-string/c x)
        (or (not (string? x))
            (λ (blame)
              (raise-blame-error
               blame x
               `(";\n boolean switches do not parse argument strings"
                 expected: "(not/c string?)" given: "~e")
               x))))
      (flat-contract-with-explanation not-string/c)))

  (define bool-flag-parameter/c
    (parameter/c not-string/c boolean?))

  (define (nat-flag-parameter/c parsed-cli-arg/c multi?)
    (parameter/c (or/c string?
                       parsed-cli-arg/c
                       ; avoid confusion with and/c
                       (if multi? (listof parsed-cli-arg/c) #f))
                 (if multi?
                     (listof parsed-cli-arg/c)
                     (or/c #f parsed-cli-arg/c))))

  (define make-flag-parameter
    (case-lambda
      [(name) ; This is a boolean switch: nothing to parse.
       (make-parameter #f (λ (x) (and x #t)) name)]
      [(name parsed-arg-ok? init)
       (define (parse-string str)
         (define n (string->number str))
         (unless (parsed-arg-ok? n)
           (raise-arguments-error 'indent-test-data-cli
                                  "bad argument to switch"
                                  "switch" name
                                  "expected" parsed-arg-ok?
                                  "given" (unquoted-printing-string str)))
         n)
       (define (parse-new x)
         (if (string? x)
             (parse-string x)
             x))
       (define guard
         (cond
           [(null? init) ; List parameter: replace list or cons on new value.
            (λ (x)      #; (this atom) #|is short for|# #;(this (cons (parse-new atom) (this)))
              (if (list? x)
                  x
                  (cons (parse-new x) (this))))]
           [else
            parse-new]))
       (define this
         (make-parameter init guard name))
       this]))


  (begin-for-syntax
    (define-splicing-syntax-class flag-formal
      #:description "flag argument clause"
      #:attributes [kw name init-expr fun-arg/c definitions]
      (pattern
        (~seq (~describe "function formal"
                         (~seq kw:keyword
                               (~or* (~describe "argument name"
                                                name:id)
                                     (~describe "identifier with initializer"
                                                [name:id custom-init-expr:expr]))))

              (~describe "parameter description"
                         (~seq
                          --name:id
                          (~optional (~seq (~optional (~and #:multi multi?))
                                           (~var parsed-cli-arg/c-expr
                                                 (expr/c #'flat-contract?
                                                         #:name "cli contract expression")))))))
        #:attr parsed-cli-arg/c (and (attribute parsed-cli-arg/c-expr.c)
                                     (format-id #'foo "~a/parsed/c" #'--name))
        #:attr fun-arg/c #`(~? (~? (and 'multi? (listof parsed-cli-arg/c))
                                   (or/c #f parsed-cli-arg/c))
                               not-string/c)
        #:with (~var param-expr (expr/c #'(~? (nat-flag-parameter/c parsed-cli-arg/c (~? 'multi? #f))
                                              bool-flag-parameter/c)
                                        #:name "flag parameter"))
        (quasisyntax/loc #'--name
          (make-flag-parameter '--name
                               (~? (~@ parsed-cli-arg/c (~? (and 'multi? null) #f)))))
        #:attr init-expr #`(~? custom-init-expr (--name))
        #:attr definitions #`(begin
                               (~? (define parsed-cli-arg/c parsed-cli-arg/c-expr.c))
                               (define --name param-expr.c)))))


  (define-simple-macro (define-main (~describe "function header"
                                               (name:id arg:flag-formal ...))
                         body:expr ...+)
    #:with (~var lambda-expr (expr/c #'(->* [] [(~@ arg.kw arg.fun-arg/c) ...] any)))
    (quasisyntax/loc this-syntax
      (λ ({~@ arg.kw [arg.name arg.init-expr]} ...)
        body ...))
    (begin arg.definitions ...
           (define name lambda-expr.c)))


  (define-main (indent-test-data-cli
                #:add-from-nat to-add --add-from-nat #:multi natural-number/c
                #:add-random num-random-to-add --add-random exact-positive-integer?
                #:redo-python? redo-python? --redo-python
                #:validate to-validate --validate #:multi natural-number/c
                #:validate-all? [validate-all? (or (--validate-all)
                                                   (and (null? to-validate)
                                                        (null? to-add)
                                                        (not num-random-to-add)))]
                --validate-all)
    (for-each add-from-nat to-add)
    (when num-random-to-add
      (define done (add-random num-random-to-add #:redo-python? redo-python?))
      (for-each display
                (if (= 1 num-random-to-add)
                    @list{Added data in randomly chosen directory "@car[done]/".@"\n"}
                    (cons "Added data in these randomly-chosen directories:\n  "
                          (add-between done "/\n  " #:after-last '("/\n"))))))
    (cond
      [(or validate-all?
           (and (null? to-validate)
                (null? to-add)
                (not num-random-to-add)))
       (validate-all #:redo-python? redo-python?)]
      [else
       (validate-list #:redo-python? redo-python? to-validate)])))

;
;
;
;
;
;
;
;
;
;    ;;;;    ;; ;;;   ;;   ;;  ;;;;;; ;;;
;   ;;   ;   ;;;  ;   ;;   ;;  ;;  ;;;  ;;
;   ;    ;;  ;;   ;;  ;;   ;;  ;;   ;;   ;
;   ;    ;;  ;;   ;;  ;;   ;;  ;;   ;;   ;
;   ;;;;;;;  ;;   ;;  ;;   ;;  ;;   ;;   ;
;   ;        ;;   ;;  ;;   ;;  ;;   ;;   ;
;   ;        ;;   ;;  ;;   ;;  ;;   ;;   ;
;   ;;   ;   ;;   ;;   ;  ;;;  ;;   ;;   ;
;    ;;;;    ;;   ;;   ;;; ;;  ;;   ;;   ;
;
;
;
;
;




(module enum racket
  (provide (all-from-out data/enumerate/lib)
           (all-defined-out))
  (require data/enumerate/lib
           racket/symbol)
  (define portable-indent/e ; levels above 10 are not reproducible in JS
    (or/e (single/e #\tab) (except/e (below/e 11) 0)))
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
    (letrec ([compound/e
              (delay/e (or/e (cons array/e list?)
                             (cons object/e hash?)))]
             [expr/e
              (or/e (cons compound/e (or/c list? hash?))
                    (cons atom/e (or/c exact-integer?
                                       inexact-rational?
                                       boolean?
                                       string?
                                       jsnull?)))]
             [array/e (knot-array/e expr/e)]
             [object/e (knot-object/e expr/e)])
      ;; match the order of our arguments
      (values object/e array/e atom/e compound/e expr/e
              (list/e portable-indent/e compound/e))))
  (define ((knot-object/e symbol/e) expr/e)
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
            (cons/e (except/e (set/e symbol/e) (set))
                    natural/e))))
  (define-values [jsobject/e
                  jsarray/e
                  jsatom/e compound-jsexpr/e jsexpr/e
                  test-datum/e]
    (knot-json/e (knot-object/e symbol/e)
                 list/e
                 (or/e jsnull/e
                       bool/e
                       string/e
                       integer/e
                       inexact-rational/e)))
  ;; For random generation, constrain symbols, strings, and integers 
  ;; to focus on variation in arrays and objects.
  ;; DO NOT change `test-datum/e`, which enumerates all possibilities.
  (define constrained-char/e
    (let ([base/e (apply except/e
                         (range/e 38 126)
                         (map char->integer '(#\: #\[ #\] #\{ #\})))])
      (map/e integer->char
             char->integer
             base/e
             #:contract (and/c char? (property/c char->integer
                                                 (enum-contract base/e))))))
  (define constrained-string/e
    (let* ([max-len 5] ; related to https://github.com/racket/racket/issues/4684
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
  (define constrained-symbol/e
    (map/e string->symbol
           symbol->immutable-string
           constrained-string/e
           #:contract (and/c symbol? (property/c symbol->immutable-string
                                                 (enum-contract constrained-string/e)))))
  (define int32/e
    (range/e (- (expt 2 31))
             (- (expt 2 31) 1)))
  (define-values [constrained-object/e
                  constrained-array/e
                  constrained-atom/e constrained-compound-jsexpr/e constrained-jsexpr/e
                  constrained-test-datum/e]
    (knot-json/e (knot-object/e constrained-symbol/e)
                 list/e
                 (or/e jsnull/e
                       bool/e
                       constrained-string/e
                       int32/e
                       inexact-rational/e)))
  
  
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
    (in-cycle (from-nat indent-order/e which))))

;; ---------------------------------------------------------------------------------
(module dynamic-enum racket
  (require syntax/modresolve
           (for-syntax racket/base syntax/transformer))
  (define enum-mpi
    (module-path-index-join '(submod ".." enum) (variable-reference->module-path-index
                                                 (#%variable-reference))))
  (define-syntax-rule (define/provide id ...)
    (begin (provide id ...)
           (define (get sym)
             (if (module-declared? enum-mpi 'load)
                 (dynamic-require enum-mpi sym)
                 (error "FATAL ERROR: you need to un-comment" (resolve-module-path-index enum-mpi))))
           (define-syntax id (make-variable-like-transformer #'(get 'id))) ...))
  (define/provide
    test-datum/e constrained-compound-jsexpr/e portable-indent/e
    in-indent-order-cycle random-element to-nat from-nat))


;
;
;
;
;
;
;
;                            ;
;                            ;
;    ;;;;  ;;    ;   ;;;;  ;;;;;   ;;;;    ;;;;;; ;;;
;   ;   ;   ;    ;  ;   ;    ;    ;;   ;   ;;  ;;;  ;;
;   ;       ;   ;;  ;        ;    ;    ;;  ;;   ;;   ;
;   ;;      ;;  ;   ;;       ;    ;    ;;  ;;   ;;   ;
;    ;;;;    ;  ;    ;;;;    ;    ;;;;;;;  ;;   ;;   ;
;       ;;   ;  ;       ;;   ;    ;        ;;   ;;   ;
;       ;;   ; ;;       ;;   ;    ;        ;;   ;;   ;
;   ;   ;;    ;;    ;   ;;   ;    ;;   ;   ;;   ;;   ;
;    ;;;;     ;;     ;;;;    ;;;   ;;;;    ;;   ;;   ;
;             ;;
;             ;
;             ;
;           ;;;
;


(module system racket
  (provide node-write-json
           python-write-json)
  (require json)
  (define portable-indent/c ; <--------------------------- Should this live here?
    (or/c #\tab (integer-in 1 10)))
  (define/contract ((make-runner name #:try-first [try-first #f]
                                 make-args)
                    indent js)
    (->* [string?
          (-> string? string? (listof string?))]
         [#:try-first (or/c #f string?)]
         (-> portable-indent/c jsexpr? any))
    (define args
      (make-args
       (if (eqv? #\tab indent)
           @quote{"\t"}
           (number->string indent))
       ;; no easy JS equivalent to sort_keys, so make Racket do it
       (jsexpr->string js)))
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
           name prog))
    (newline)
    (cons prog args))
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
             @string-append{
 import json
 import sys
 json.dump(json.loads(sys.argv[-1]),
 @""       sys.stdout,
 @""       indent=@|indent|,
 @""       sort_keys=True,
 @""       ensure_ascii=False)
}
             json)))))

;
;
;
;
;
;
;
;                                                     ;
;        ;;                                           ;
;    ;;;;    ;;;;    ;; ;;;    ;;;;    ;; ;   ;;;;  ;;;;;   ;;;;
;   ;;  ;;  ;;   ;   ;;;  ;   ;;   ;   ;;;   ;;  ;;   ;    ;;   ;
;   ;    ;  ;    ;;  ;;   ;;  ;    ;;  ;;         ;   ;    ;    ;;
;   ;    ;  ;    ;;  ;;   ;;  ;    ;;  ;;         ;   ;    ;    ;;
;   ;;   ;  ;;;;;;;  ;;   ;;  ;;;;;;;  ;;     ;;;;;   ;    ;;;;;;;
;    ;;;;   ;        ;;   ;;  ;        ;;    ;    ;   ;    ;
;    ;      ;        ;;   ;;  ;        ;;    ;    ;   ;    ;
;   ;       ;;   ;   ;;   ;;  ;;   ;   ;;    ;  ;;;   ;    ;;   ;
;   ;;;;;;   ;;;;    ;;   ;;   ;;;;    ;;    ;;;; ;;  ;;;   ;;;;
;   ;    ;;
;   ;     ;
;   ;    ;;
;    ;;;;;
;

;(module generate-and-validate racket/base
(provide add-from-nat
         add-random
         validate
         validate-list
         validate-all)

(require 'dynamic-enum
         'system
         rackunit)
(void (putenv "NODE" "/gnu/store/ljcqb9w28xsqgd992gxm33xz7s3x190v-node-14.19.3/bin/node"))

(define (datum-from-nat n)
  (from-nat test-datum/e n))
(define (dir-for-nat n)
  (build-path indent-test-data/ (number->string n)))

(define (pretty-write-to-file x pth)
  (call-with-output-file* pth
    (λ (out)
      (pretty-write x out))))

(define-values [node-write-for-nat
                python-write-for-nat]
  (let ()
    (define (rm-f pth)
      (delete-directory/files pth #:must-exist? #f))
    (define (run-writer name proc nat #:exists [exists 'error])
      (parameterize ([current-directory (dir-for-nat nat)])
        (define json-pth (string-append name ".json"))
        (define rktd-pth (string-append "args." name ".rktd"))
        (when (eq? exists 'redo)
          (for-each rm-f (list json-pth rktd-pth)))
        (unless (and (eq? exists 'ignore)
                     (file-exists? json-pth))
          (rm-f rktd-pth)
          (pretty-write-to-file (with-output-to-file json-pth
                                  (λ ()
                                    (apply proc (datum-from-nat nat))))
                                rktd-pth))))
    (define (node-write-for-nat nat)
      (run-writer "node" node-write-json nat))
    (define (python-write-for-nat nat #:redo-python? [redo-python? #f])
      (run-writer "python" python-write-json nat #:exists (if redo-python?
                                                              'redo
                                                              'ignore)))
    (values node-write-for-nat
            python-write-for-nat)))

(define (add-from-nat nat)
  (define dir (dir-for-nat nat))
  (make-directory dir)
  (pretty-write-to-file (datum-from-nat nat)
                        (build-path dir "datum.rktd"))
  (node-write-for-nat nat)
  (validate nat))

(define (file->jsexpr pth)
  (string->jsexpr ; ensure whole file is consumed
   (file->string pth)))

(define (validate nat #:redo-python? [redo-python? #f])
  (parameterize ([current-directory (dir-for-nat nat)])
    (python-write-for-nat nat #:redo-python? redo-python?)
    (match-define (and datum (list indent jsexpr))
      (datum-from-nat nat))
    (test-case
     (string-append (number->string nat) "/")
     (test-equal? "datum.rktd contains correct value"
                  (file->value "datum.rktd")
                  datum)
     (test-equal? "node.json contains correct value"
                  (file->jsexpr "node.json")
                  jsexpr)
     (test-equal? "python.json is identical to node.json"
                  (file->string "python.json")
                  (file->string "node.json")))))

(define (validate-list nats #:redo-python? [redo-python? #f])
  (for-each (λ (n)
              (validate n #:redo-python? redo-python?))
            nats))

(define (validate-all #:redo-python? [redo-python? #f])
  (validate-list #:redo-python? redo-python?
                 (parameterize ([current-directory indent-test-data/])
                   (for/list ([pth (in-list (directory-list))]
                              #:when (directory-exists? pth))
                     (string->number (path->string pth))))))

(define (add-random num-random-to-add #:redo-python? [redo-python? #f])
  (for/list ([i (in-inclusive-range 1 num-random-to-add)]
             [indent (in-indent-order-cycle)])
    ;; Use all indentations as equally with as possible,
    ;; with a random selection for the remainder.
    (let retry ()
      (define jsexpr
        (random-element constrained-compound-jsexpr/e))
      (define nat (to-nat test-datum/e (list indent jsexpr)))
      (cond
        [(directory-exists? (dir-for-nat nat))
         (for-each display
                   `@{
         Encountered previously added datum @,nat durring @;
         @,@(if (= 1 num-random-to-add)
                '()
                @list{the @n->th[i] of @|num-random-to-add| steps of })@;
         random generation.
         Validating it before moving on.@"\n"})
         (validate nat #:redo-python? redo-python?)
         (retry)]
        [else
         (add-from-nat nat)
         nat]))))
