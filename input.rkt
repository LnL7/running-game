#lang racket/base
(require "lib/canvas.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeInput)



(define (MakeInput)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((jump)   player_jump)
        ((strafe) player_strafe)
        (else
          (method_missing msg 'Input)))
      args))

  (define (player_jump is_jumping? start_jumping velocity)
    (on-key! #\w (lambda ()
                   (unless (is_jumping?)
                     (start_jumping)
                     (velocity 'add! JUMP_VELOCITY))))
    (on-key! #\s (lambda ()
                   (when (is_jumping?)
                     (velocity 'add! FALL_VELOCITY)))))

  (define (player_strafe position)
    (on-key! #\d (lambda ()
                   (position 'add! STRAFE_RIGHT_VELOCITY)))
    (on-key! #\a (lambda ()
                   (position 'add! STRAFE_LEFT_VELOCITY))))

  ;; Private

  dispatch)


(define JUMP_VELOCITY         (MakeVelocity 0 100))
(define FALL_VELOCITY         (MakeVelocity 0 -10))
(define STRAFE_RIGHT_VELOCITY (MakeVelocity 1 0))
(define STRAFE_LEFT_VELOCITY  (MakeVelocity -1 0))
