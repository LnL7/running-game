#lang racket/base
(require "shape.rkt")
(require "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeObstacle)



(define (MakeObstacle position width height)
  (let ((_position position)
        (_width    width)
        (_height   height)
        (_velocity (MakeVelocity 0 0)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((render)  render)
          ((update!) update!)
          (else
            (method_missing msg dispatch)))
        args))

    (define (render engine)
      (let ((shape (MakeRectangle _position _velocity _width _height COLOR)))
        (shape 'render engine)))

    (define (update! engine)
      (let ((shape (MakeRectangle _position _velocity _width _height)))
        (shape 'render engine)))

    ;; Private

    (define (get_position) (_position 'copy))
    (define (get_velocity) (_velocity 'copy))

    dispatch))


(define COLOR 'black)
