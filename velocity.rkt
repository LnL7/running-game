#lang racket/base
(require "helpers.rkt")
(provide (all-from-out "helpers.rkt")
         MakeVelocity)



(define (MakeVelocity horizontal vertical)
  (let ((_horizontal horizontal)
        (_vertical vertical))
    (define (dispatch msg)
      (cond
        ((eq? msg 'horizontal) get_horizontal)
        ((eq? msg 'vertical)   get_vertical)
        ((eq? msg 'scale!)     scale_number!)
        ((eq? msg 'add!)       add_velocity!)
        ((eq? msg 'render)     render)
        (else
          (error "method missing ~a" dispatch))))

      (define (get_horizontal) _horizontal)
      (define (get_vertical)   _vertical)

      (define (scale_number! scale)
        (unless (= scale 1)
          (begin
            (set! _horizontal (* scale _horizontal))
            (set! _vertical (* scale _vertical))))
        dispatch)

      (define (add_velocity! vel)
        (set! _horizontal (+ (send vel 'horizontal) _horizontal))
        (set! _vertical   (+ (send vel 'vertical) _vertical))
        dispatch)

      (define (render engine)
        (send engine 'velocity dispatch))

      dispatch))

