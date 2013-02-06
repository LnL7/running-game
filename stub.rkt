#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeStub)



(define (MakeStub #:log [_log (MakeLogger)] . methods)
  (let ((_ctr 0))
    (define (dispatch msg . args)
      (apply
        (let __iter ((lst methods))
          (if (null? lst)
            (_log 'fatal "method missing" msg dispatch)
            (let ((head (car lst)))
              (if (eq? msg head)
                stub
                (__iter (cdr lst))))))
        args))

    (define (stub . _)
      (set! _ctr (+ _ctr 1))
      (_log 'warn "stubbed with, " _)
      _ctr)

    (_log 'debug "initialized Stub")

    dispatch))
