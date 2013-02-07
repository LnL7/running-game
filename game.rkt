#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "player.rkt"
         "obstacle-collection.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-player    (MakePlayer kPlayerPosition #:log -log))
        (-obstacles (MakeObstacleCollection #:log -log))
        (-display   (MakeDisplay #:log -log))
        (-physics   (MakePhysics #:log -log))
        (-input     (MakeInput #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((start) start)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (start)
      (-player 'physics -physics)
      (-player 'input -input)
      (-obstacles 'fill!)
      (start-game-loop game-loop #t))


    ;; Private

    (define (game-loop delta)
      (-player    'render -display)
      (-obstacles 'render -display)
      (-player    'update! delta -physics)
      (-obstacles 'update! delta -physics))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass          'Game)
(define kPlayerPosition (MakePosition 200 200))
