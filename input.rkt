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
                     (-log 'debug "key-down" 'w)
                     (start-jumping)
                     (velocity 'add! kJumpVelocity))))
    (on-key! #\s (lambda ()
                   (when (is-jumping?)
                     (-log 'debug "key-down" 's)
                     (velocity 'add! kFallVelocity)))))

  (define (player-strafe velocity)
    (on-key! #\d (lambda ()
                   (velocity 'horizontal! kRightStrafeSpeed)))
    (on-key! #\a (lambda ()
                   (velocity 'horizontal! kLeftStrafeSpeed))))


  ;; Private

  (-log 'debug "initialized Logger")

  dispatch)


(define kJumpVelocity        (MakeVelocity 0 100))
(define kFallVelocity        (MakeVelocity 0 -10))
(define kRightStrafeSpeed    20)
(define kLeftStrafeSpeed     -20)
