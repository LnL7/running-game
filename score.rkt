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
        ((->)      (apply encode args))
        ((<-)      (apply decode args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Score)

    (define (add-current)
      (set! -current (+ -current 1)))

    (define (end-current)
      (when (> -current -highest)
        (set! -highest -current))
      (set! -current 0))

    (define (encode) (vector -current -highest))
    (define (decode vect)
      (set! -current (vector-ref vect 0))
      (set! -highest (vector-ref vect 1)))

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))
