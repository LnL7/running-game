#lang racket/base
(require "shape.rkt")
(provide (all-from-out "shape.rkt")
         MakePlayer)



(define (MakePlayer position)
  (let ((_position position)
        (_velocity (MakeVelocity 0 0)))
    (define (dispatch msg . args)
      (apply
        (cond
          ((eq? msg 'render)  render)
          ((eq? msg 'update!) update!)
          (else
            (error msg "method missing ~a" dispatch)))
        args))

    (define (render engine)
      (let ((shape (MakeEllipse _position (MakeVelocity 0 0) SIZE SIZE COLOR)))
        (shape 'render engine)))

    (define (update! engine)
      (let ((shape (MakeRectangle _position _velocity SIZE SIZE)))
        (shape 'update! engine)))

    ;; Private

    (define (get_position) (_position 'copy))
    (define (get_velocity) (_velocity 'copy))

    dispatch))


(define SIZE 100)
(define COLOR 'blue)
