#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeInput)



(define (MakeInput #:log [-log (MakeLogger)])
  (let ((-game #f))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((game!)  set-game!)
          ((world)  world-start)
          ((jump)   player-jump)
          ((strafe) player-strafe)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (set-game! game) (set! -game game))


    (define (world-start)
      (on-key! #\return (lambda () (when -game (-game 'world)))))

    (define (player-jump is-jumping? start-jumping velocity)
      (let ((jump (lambda ()
                     (unless (is-jumping?)
                       (start-jumping)
                       (velocity 'add! kJumpVelocity))))
            (fall (lambda ()
                     (when (is-jumping?)
                       (velocity 'add! kFallVelocity)))))

        (on-key! 'up jump)
        (on-key! #\w jump)
        (on-key! 'down fall)
        (on-key! #\s fall)))

    (define (player-strafe velocity)
      (let ((left  (lambda () (velocity 'horizontal! kLeftStrafeSpeed)))
            (right (lambda () (velocity 'horizontal! kRightStrafeSpeed))))
        (on-key! 'left left)
        (on-key! #\a left)
        (on-key! 'right right)
        (on-key! #\d right)))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass               'Input)
(define kJumpVelocity        (MakeVelocity 0 100))
(define kFallVelocity        (MakeVelocity 0 -10))
(define kRightStrafeSpeed    20)
(define kLeftStrafeSpeed     -20)
