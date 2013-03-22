#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeStub)


(define (MakeStub #:log [-log (MakeLogger)] . -methods)
  (define (Stub msg . args)
    (apply
      (let --iter ((lst -methods))
        (if (null? lst)
          (-log 'fatal "method missing" msg dispatch)
          (let ((head (car lst)))
            (cond
              ((eq? msg head)       stub)
              ((method msg head) => (lambda (m) m))
              (else
                (--iter (cdr lst)))))))
      args))
  (define dispatch Stub)

  (define (stub . args) #t)

  ;; Private
  (define (method msg lst)
    (and
      (pair? lst)
      (eq? (car lst) msg)
      (let ((res (cadr lst)))
        (if (procedure? res)
          res
          (lambda args res)))))

  (-log 'debug "initialized" dispatch)
  dispatch)
