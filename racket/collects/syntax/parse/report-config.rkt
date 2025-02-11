#lang racket/kernel

(#%declare #:cross-phase-persistent)

(#%provide report-configuration?
           current-report-configuration)

(define-values (report-configuration?)
  (lambda (v)
    (let-values ([(procedure1?)
                  (lambda (key)
                    (let-values ([(p) (hash-ref v key #f)])
                      (if (procedure? p)
                          (procedure-arity-includes? p 1)
                          #f)))])
      (if (hash? v)
          (if (immutable? v)
              (if (procedure1? 'literal-to-what)
                  (if (procedure1? 'literal-to-string)
                      (if (procedure1? 'datum-to-what)
                          (procedure1? 'datum-to-string)
                          #f)
                      #f)
                  #f)
              #f)
          #f))))

(define-values (current-report-configuration)
  (make-parameter (hasheq 'literal-to-what (lambda (v)
                                             '("identifier" "identifiers"))
                          'literal-to-string (lambda (v)
                                               (format "`~s'" (if (syntax? v)
                                                                  (syntax-e v)
                                                                  v)))
                          'datum-to-what (lambda (v)
                                           (if (symbol? v)
                                               '("literal symbol" "literal symbols")
                                               '("literal" "literals")))
                          'datum-to-string (lambda (v)
                                             (if (symbol? v)
                                                 (format "`~s'" v)
                                                 (format "~s" v))))
                  (lambda (v)
                    (if (report-configuration? v)
                        v
                        (raise-argument-error 'current-report-configuration "report-configuration?" v)))))
