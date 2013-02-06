#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeVelocity)



(define (MakeVelocity horizontal vertical #:log [-log (MakeLogger)])
  (let ((-horizontal horizontal)
        (-vertical   vertical))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((horizontal)  get-horizontal)
          ((vertical)    get-vertical)
          ((horizontal!) set-horizontal!)
          ((vertical!)   set-vertical!)
          ((copy)        copy-velocity)
          ((scale!)      scale-number!)
          ((add!)        add-velocity!)
          ((render)      render)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (get-horizontal) -horizontal)
    (define (get-vertical)   -vertical)

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

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Velocity)
