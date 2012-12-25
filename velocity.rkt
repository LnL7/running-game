#lang racket/base
(require "helpers.rkt")
(provide MakeVelocity)



(define (MakeVelocity horizontal vertical)
  (let ((_horizontal horizontal)
        (_vertical vertical))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((horizontal)  get_horizontal)
          ((vertical)    get_vertical)
          ((horizontal!) set_horizontal!)
          ((vertical!)   set_vertical!)
          ((copy)        copy_velocity)
          ((scale!)      scale_number!)
          ((add!)        add_velocity!)
          ((render)      render)
          (else
            (method_missing msg dispatch)))
        args))

    (define (get_horizontal) _horizontal)
    (define (get_vertical)   _vertical)

    (define (set_horizontal! horizontal) (set! _horizontal horizontal))
    (define (set_vertical! vertical)     (set! _vertical vertical))

    (define (copy_velocity) (MakeVelocity _horizontal _vertical))

    (define (scale_number! scale)
      (unless (= scale 1)
        (set! _horizontal (* scale _horizontal))
        (set! _vertical (* scale _vertical)))
      dispatch)

    (define (add_velocity! vel)
      (set! _horizontal (+ (vel 'horizontal) _horizontal))
      (set! _vertical   (+ (vel 'vertical) _vertical))
      dispatch)

    (define (render engine)
      (engine 'velocity dispatch))

    dispatch))
