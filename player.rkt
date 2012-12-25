#lang racket/base
(require "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakePlayer)



(define (MakePlayer position)
  (let ((_is_jumping    #t)
        (_display_shape #f)
        (_physics_shape #f)
        (_position      position)
        (_velocity      NULL_VELOCITY))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((input)   input)
          ((render)  render)
          ((update!) update!)
          (else
            (method_missing msg dispatch)))
        args))

    (define (input engine)
      (engine 'strafe _velocity)
      (engine 'jump get_is_jumping? start_jumping _velocity))

    (define (render engine)
      (unless _display_shape (set! _display_shape (MakeImage _position SIZE SIZE PATH)))
      (_display_shape 'render engine))

    (define (update! engine)
      (unless _physics_shape (set! _physics_shape (MakeRectangle _position SIZE SIZE)))
      (lambda args
        (apply (engine 'gravity _velocity _position end_jumping) args)
        (apply (_physics_shape 'move! engine _velocity) args)))

    ;; Private

    (define (get_is_jumping?) _is_jumping)
    (define (end_jumping)     (set! _is_jumping #f))
    (define (start_jumping)   (set! _is_jumping #t))

    dispatch))


(define NULL_VELOCITY    (MakeVelocity 0 0))
(define SIZE             50)
(define PATH             "resources/player.png")
