#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeStub)



(define (MakeStub #:log [-log (MakeLogger)] . methods)
  (define (dispatch msg . args)
    (apply
      (let --iter ((lst methods))
        (if (null? lst)
          (-log 'fatal "method missing" msg kClass)
          (let ((head (car lst)))
            (cond
              ((eq? msg head)       stub)
              ((method msg head) => (lambda (m) m))
              (else
                (--iter (cdr lst)))))))
      args))


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

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass 'Stub)
