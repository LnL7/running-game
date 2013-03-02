#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeScore)



(define (MakeScore #:log [-log (MakeLogger)])
  (let ((-current    0)
        (-highest    0))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((->)      encode)
          ((<-)      decode)
          ((current) get-current)
          ((highest) get-highest)
          ((add)     add-current)
          ((end)     end-current)
          ((render)  render)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (get-current) -current)
    (define (get-highest) -highest)


    (define (encode) (vector -current -highest))
    (define (decode vect)
      (set! -current (vector-ref vect 0))
      (set! -highest (vector-ref vect 1)))


    (define (add-current)
      (set! -current (+ -current 1)))

    (define (end-current)
      (when (> -current -highest)
        (set! -highest -current))
      (set! -current 0))

    (define (render engine)
      (engine 'score dispatch))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Score)
