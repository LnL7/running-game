#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeScore)


(define (MakeScore #:log [-log (MakeLogger)])
  (let ((-current 0)
        (-highest 0))
    (define (Score msg . args)
      (case msg
        ((current) -current)
        ((highest) -highest)
        ((add)     (apply add-current args))
        ((end)     (apply end-current args))
        ((encode)  (apply encode args))
        ((decode)  (apply decode args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Score)

    (define (add-current)
      (set! -current (+ -current 1)))

    (define (end-current)
      (when (> -current -highest)
        (set! -highest -current))
      (set! -current 0))

    (define (encode)
      (vector 'score
              -highest))

    (define (decode vect)
      (unless (eq? (vector-ref vect 0) 'score) (-log 'warn "wrong encoded vector for" dispatch))
      (set! -highest (vector-ref vect 1)))

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))
