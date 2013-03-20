#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeVelocity)



(define (MakeVelocity -horizontal -vertical #:log [-log (MakeLogger)])
  (let ()
    (define (dispatch msg . args)
      (case msg
        ((horizontal)  -horizontal)
        ((vertical)    -vertical)
        ((horizontal!) (apply set-horizontal! args))
        ((vertical!)   (apply set-vertical! args))
        ((copy)        (apply copy-velocity args))
        ((scale!)      (apply scale-number! args))
        ((add!)        (apply add-velocity! args))
        ((render)      (apply render args))
        (else
          (-log 'fatal "method missing" msg dispatch))))

    (define (set-horizontal! horizontal) (set! -horizontal horizontal))
    (define (set-vertical! vertical)     (set! -vertical vertical))

    (define (copy-velocity) (MakeVelocity -horizontal -vertical))

    (define (scale-number! scale)
      (unless (= scale 1)
        (set! -horizontal (* scale -horizontal))
        (set! -vertical (* scale -vertical)))
      dispatch)

    (define (add-velocity! vel)
      (set! -horizontal (+ (vel 'horizontal) -horizontal))
      (set! -vertical   (+ (vel 'vertical) -vertical))
      dispatch)

    (define (render engine)
      (engine 'velocity dispatch))

    ;; Private
    ; (-log 'debug "initialized" dispatch)
    dispatch))
