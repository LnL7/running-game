#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeInput)



(define (MakeInput #:log [-log (MakeLogger)])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((jump)   player-jump)
        ((strafe) player-strafe)
        (else
          (-log 'fatal "method missing" msg 'Input)))
      args))

  (define (player-jump is-jumping? start-jumping velocity)
    (on-key! #\w (lambda ()
                   (unless (is-jumping?)
                     (start-jumping)
                     (velocity 'add! kJumpVelocity))))
    (on-key! #\s (lambda ()
                   (when (is-jumping?)
                     (velocity 'add! kFallVelocity)))))

  (define (player-strafe position)
    (on-key! #\d (lambda ()
                   (position 'add! kStrafeRightVelocity)))
    (on-key! #\a (lambda ()
                   (position 'add! kStrafeLeftVelocity))))


  ;; Private

  (-log 'debug "initialized Logger")

  dispatch)


(define kJumpVelocity        (MakeVelocity 0 100))
(define kFallVelocity        (MakeVelocity 0 -10))
(define kStrafeRightVelocity (MakeVelocity 1 0))
(define kStrafeLeftVelocity  (MakeVelocity -1 0))
