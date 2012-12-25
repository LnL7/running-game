#lang racket/base
(require "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakePlayer)



(define (MakePlayer position)
  (let ((_display_shape #f)
        (_physics_shape #f)
        (_position      position)
        (_velocity      NULL_VELOCITY))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((render)  render)
          ((update!) update!)
          (else
            (method_missing msg dispatch)))
        args))

    (define (render engine)
      (unless _display_shape (set! _display_shape (MakeImage _position SIZE SIZE PATH)))
      (_display_shape 'render engine))

    (define (update! engine)
      (unless _physics_shape (set! _physics_shape (MakeRectangle _position SIZE SIZE)))
      (lambda args
        (apply (engine 'gravity _velocity) args)
        (apply (_physics_shape 'move! engine _velocity) args)))

    ;; Private

    dispatch))


(define NULL_VELOCITY    (MakeVelocity 0 0))
(define JUMP_VELOCITY    (MakeVelocity 0 10))
(define SIZE             25)
(define PATH             "resources/player.png")
