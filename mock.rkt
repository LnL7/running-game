#lang racket/base
(require "logger.rkt"
         "stub.rkt"
         "helpers.rkt")
(provide MakeMock)


(define (MakeMock #:log [-log (MakeLogger)] . methods)
  (let ((-messages '())
        (-stub      (keyword-apply MakeStub '(#:log) (list -log) methods)))
    (define (Mock msg . args)
      (cond
        ((eq? msg 'messages) (apply get-messages args))
        (else
          (-log 'debug "received" msg dispatch)
          (set! -messages (cons msg -messages))
          (apply -stub msg args))))
    (define dispatch Mock)

    ;; Private
    (define (get-messages)
      (let ((lst -messages))
        (set! -messages '())
        (reverse lst)))

    ; (-log 'debug "initialized" dispatch)
    dispatch))
