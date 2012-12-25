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
        (when (<= (pos 'y) FLOOR_HEIGHT)
          (pos 'y! FLOOR_HEIGHT)
          (velocity 'vertical! BOUNCE_SPEED))
        (vel 'scale! (/ delta 100))
        (pos 'move! vel))))

  (define (velocity_gravity velocity)
    (lambda (delta)
      (let ((vel (GRAVITY_VELOCITY 'copy)))
        (velocity 'add! (vel 'scale! (/ delta 100))))))

  dispatch)


(define FLOOR_HEIGHT     20)
(define BOUNCE_SPEED     5)
(define GRAVITY_VELOCITY (MakeVelocity 0 -0.94))
