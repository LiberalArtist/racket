#lang racket/base

(require racket/contract
         racket/tcp)

(provide
 (contract-out
  [start-server (server-proc/c (-> void?))]
  [run-server (server-proc/c void?)]))

(define (server-proc/c res/c)
  (->* (evt? (-> input-port? output-port? any))
       (#:max-concurrent   (or/c +inf.0 natural-number/c)
        #:accept           (-> any/c (values input-port? output-port?))
        #:close            (-> any/c any)
        #:make-timeout-evt (-> thread? input-port? output-port? boolean? evt?))
       res/c))

(define (start-server listen-evt handle
                      #:max-concurrent   [max-concurrent +inf.0]
                      #:accept           [accept tcp-accept]
                      #:close            [close tcp-close]
                      #:make-timeout-evt [make-timeout-evt (λ (_thd _in _out _break-sent?) never-evt)])
  (define can-break?
    (break-enabled))
  (define paramz
    (current-parameterization))
  (define server-thd
    (thread
     (lambda ()
       (dynamic-wind
         void
         (lambda ()
           (with-handlers ([exn:break? void])
             (let loop ([in-progress 0])
               (loop
                (with-handlers ([exn:fail:network?
                                 (λ (e)
                                   (begin0 in-progress
                                     ((error-display-handler)
                                      (format "Connection error: ~a" (exn-message e))
                                      e)))])
                  (sync/enable-break
                   ;; Deduct completed connections from `in-progress`
                   (handle-evt
                    (thread-receive-evt)
                    (lambda (_)
                      (let drain-loop ([in-progress in-progress])
                        (if (thread-try-receive)
                            (drain-loop (sub1 in-progress))
                            in-progress))))
                   ;; Accept new connections, subject to `in-progress`
                   (handle-evt
                    (if (< in-progress max-concurrent) listen-evt never-evt)
                    (lambda (listener)
                      ;; make custodians for the new connection
                      (define supervisor-cust (make-custodian)) ; for supervisor-thd
                      (define client-cust (make-custodian supervisor-cust)) ; for resources
                      (parameterize ([current-custodian client-cust])
                        ;; disable breaks during connection set-up...
                        (parameterize-break #f
                          (define-values (in out)
                            (accept listener))
                          (define client-thd
                            (thread
                             (lambda ()
                               (call-with-parameterization
                                paramz
                                (lambda ()
                                  (break-enabled can-break?)
                                  ;; create an intermediary custodian to provent `handle`
                                  ;; from shutting down `client-cust`
                                  (parameterize ([current-custodian (make-custodian client-cust)])
                                    (handle in out)))))))
                          (define supervisor-thd
                            ;; under supervisor-cust so we can shut down client-cust
                            ;; before we report that this connection is finished,
                            ;; which means supervisor-thd must run after client-cust is shut down
                            (parameterize ([current-custodian supervisor-cust])
                              (thread
                               (lambda ()
                                 (sync client-thd (make-timeout-evt client-thd in out #f))
                                 (when (thread-running? client-thd)
                                   (break-thread client-thd)
                                   (sync client-thd (make-timeout-evt client-thd in out #t)))
                                 ;; release resources
                                 (custodian-shutdown-all client-cust)
                                 ;; report that this connection is closed
                                 (thread-send server-thd 'done void)
                                 ;; shut down ourself
                                 (custodian-shutdown-all supervisor-cust)))))
                          (add1 in-progress)))))))))))
         (lambda ()
           (close listen-evt))))))
  (lambda ()
    (break-thread server-thd)
    (thread-wait server-thd)))

(define run-server
  (let-values ([(required-kws optional-kws) (procedure-keywords start-server)])
    (procedure-reduce-keyword-arity-mask
     (make-keyword-procedure
      (lambda (kws kw-args . args)
        (parameterize-break #f
          (define stop (keyword-apply start-server kws kw-args args))
          (with-handlers ([exn:break? void])
            (sync/enable-break never-evt))
          (stop))))
     (procedure-arity-mask start-server)
     required-kws
     optional-kws
     'run-server)))
