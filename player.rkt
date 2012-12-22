#lang racket/base
(require "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakePlayer)



(define (MakePlayer position)
  (let ((_position position)
        (_velocity NULL_VELOCITY))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((render)  render)
          ((update!) update!)
          (else
            (method_missing msg dispatch)))
        args))

    (define (render engine)
      (let ((shape (MakeEllipse _position _velocity SIZE SIZE COLOR)))
        (shape 'render engine)))

    (define (update! engine)
      (let ((shape (MakeRectangle _position _velocity SIZE SIZE)))
        (shape 'update! engine)))

    ;; Private

    (define (get_position) (_position 'copy))
    (define (get_velocity) (_velocity 'copy))

    dispatch))


(define NULL_VELOCITY (MakeVelocity 0 0))
(define JUMP_VELOCITY (MakeVelocity 10 0))
(define SIZE 25)
(define COLOR "blue")
