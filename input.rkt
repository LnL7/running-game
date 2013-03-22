#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeInput)


(define (MakeInput #:log [-log (MakeLogger)])
  (let ((-game  #f)
        (-level #f))
    (define (Input msg . args)
      (case msg
        ((game!)  (apply set-game! args))
        ((level!) (apply set-level! args))
        ((world)  (apply world-start args))
        ((jump)   (apply player-jump args))
        ((strafe) (apply player-strafe args))
        ((slide)  (apply player-slide args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Input)

    ;; Properties
    (define (set-game! game)   (set! -game game))
    (define (set-level! level) (set! -level level))

    (define (world-start)
      (on-key! #\return (lambda () (when -game (-game 'world)))))

    (define (player-jump player)
      (let ((vel (player 'velocity)))
        (define (jump) (unless (player 'jumping?)
                         (player 'bounce! #t)
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

    (define (player-slide player)
      (define (slide)  (player 'bounce! #f))
      (on-key! 'down slide)
      (on-key! #\s slide))

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))
