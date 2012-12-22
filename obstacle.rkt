#lang racket/base
(require "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeObstacle)



(define (MakeObstacle position width height)
  (let ((_display_shape #f)
        (_physics_shape #f)
        (_position      position)
        (_width         width)
        (_height        height)
        (_velocity      VELOCITY))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((render)  render)
          ((update!) update!)
          (else
            (method_missing msg dispatch)))
        args))

    (define (render engine)
      (unless _display_shape (set! _display_shape (MakeRectangle _position _velocity _width _height COLOR)))
      (_display_shape 'render engine))

    (define (update! engine)
      (unless _physics_shape (set! _physics_shape (MakeRectangle _position _velocity _width _height)))
      (_physics_shape 'update! engine))

    ;; Private

    (define (get_position) (_position 'copy))
    (define (get_velocity) (_velocity 'copy))

    dispatch))


(define VELOCITY (MakeVelocity -5 0))
(define COLOR "black")
