#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "menu.rkt"
         "player.rkt"
         "obstacle-collection.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-menu      #f)
        (-player    #f)
        (-obstacles #f)
        (-display   #f)
        (-physics   #f)
        (-input     #f))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((end)   end)
          ((start) start)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (end)
      (start-game-loop menu-loop #t))

    (define (start)
      (set! -menu      (MakeMenu #:log -log))
      (set! -player    (MakePlayer kPlayerPosition #:log -log))
      (set! -obstacles (MakeObstacleCollection #:log -log))
      (set! -display   (MakeDisplay #:log -log))
      (set! -physics   (MakePhysics #:log -log))
      (set! -input     (MakeInput #:log -log))

      (-menu 'score -obstacles)
      (-physics 'game! dispatch)
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

    (define (menu-loop delta)
      (-menu 'render -display))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass          'Game)
(define kPlayerPosition (MakePosition 200 200))
