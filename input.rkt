#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeInput)


(define (MakeInput #:log [-log (MakeLogger)])
  (let ((-game  #f)
        (-level #f))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((game!)  set-game!)
          ((level!) set-level!)
          ((world)  world-start)
          ((jump)   player-jump)
          ((strafe) player-strafe)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties
    (define (set-game! game)   (set! -game game))
    (define (set-level! level) (set! -level level))

    (define (world-start)
      (on-key! #\return (lambda () (when -game (-game 'world)))))

    (define (player-jump player)
      (let ((vel (player 'velocity)))
        (define (jump) (unless (player 'jumping?)
                         (player 'jumping! #t)
                         (vel 'add! (-level 'player-jump))))
        (on-key! 'up jump)
        (on-key! #\w jump)))

    (define (player-strafe player)
      (let ((vel    (player 'velocity))
            (strafe (-level 'player-strafe)))
        (define (left)  (vel 'horizontal! (- strafe)))
        (define (right) (vel 'horizontal! (+ strafe)))
        (on-key! 'left left)
        (on-key! #\a left)
        (on-key! 'right right)
        (on-key! #\d right)))

    ;; Private
    (-log 'debug "initialized" kClass)
    dispatch))


(define kClass 'Input)
