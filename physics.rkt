#lang racket/base
(require "velocity.rkt"
         "helpers.rkt")
(provide MakePhysics)



(define (MakePhysics)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape_rectangle)
        ((gravity)   velocity_gravity)
        (else
          (method_missing msg dispatch)))
      args))

  (define (shape_rectangle rectangle velocity)
    (lambda (delta)
      (let ((pos (rectangle 'position))
            (vel (velocity 'copy)))
        (vel 'scale! (/ delta 100))
        (pos 'move! vel))))

  (define (velocity_gravity velocity position end_jumping)
    (position_reset position velocity end_jumping)
    (lambda (delta)
      (let ((vel     (velocity 'copy))
            (gravity (GRAVITY_VELOCITY 'copy)))
        (velocity 'add! (vel 'scale! (/ delta -1000)))
        (velocity 'add! (gravity 'scale! (/ delta 100))))))

  ;; Private

  (define (position_reset position velocity end_jumping)
    (when (< (position 'x) 0)
      (position 'x! 0)
      (velocity 'horizontal! 1))

    (when (> (position 'x) 750)
      (position 'x! 750)
      (velocity 'horizontal! -1))

    (when (< (position 'y) FLOOR_HEIGHT)
      (when end_jumping (end_jumping))
      (position 'y! FLOOR_HEIGHT)
      (velocity 'vertical! BOUNCE_SPEED))

    (when (> (position 'y) 550)
      (position 'y! 550)
      (velocity 'vertical! 0)))

  dispatch)


(define FLOOR_HEIGHT     0)
(define BOUNCE_SPEED     10)
(define GRAVITY_VELOCITY (MakeVelocity 0 (* -0.94 4)))
