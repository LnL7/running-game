#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeScore)



(define (MakeScore #:log [-log (MakeLogger)])
  (let ((-current 0)
        (-highest 0))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((current) get-current)
          ((highest) get-highest)
          ((add)     add-current)
          ((end)     end-current)
          ((render)  render)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (get-current) -current)
    (define (get-highest) -highest)

    (define (add-current)
      (set! -current (+ -current 1)))

    (define (end-current)
      (when (> -current -highest)
        (set! -highest -current))
      (set! -current 0))

    (define (render engine)
      (engine 'score dispatch))

    dispatch))


(define kClass 'Score)
