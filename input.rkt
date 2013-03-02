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

    (define (player-jump player)
      (let ((vel (player 'velocity)))
        (define (jump) (unless (player 'jumping?)
                         (player 'jumping! #t)
                         (vel 'add! kJumpVelocity)))
        (on-key! 'up jump)
        (on-key! #\w jump)))

    (define (player-strafe player)
      (let ((vel (player 'velocity)))
        (define (left)  (vel 'horizontal! kLeftStrafeSpeed))
        (define (right) (vel 'horizontal! kRightStrafeSpeed))
        (on-key! 'left left)
        (on-key! #\a left)
        (on-key! 'right right)
        (on-key! #\d right)))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass               'Input)
(define kJumpVelocity        (MakeVelocity 0 100))
(define kNullSpeed           0)
(define kRightStrafeSpeed    20)
(define kLeftStrafeSpeed     -20)
