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
      (start-game-loop menu-loop #t)
      dispatch)

    (define (start)
      (set! -menu      (MakeMenu #:log -log))
      (set! -player    (MakePlayer kPlayerPosition #:log -log))
      (set! -obstacles (MakeObstacleCollection #:log -log))
      (set! -display   (MakeDisplay #:log -log))
      (set! -physics   (MakePhysics #:log -log))
      (set! -input     (MakeInput #:log -log))

      (-physics 'game! dispatch)
      (-player 'physics -physics)
      (-player 'input -input)
      (-obstacles 'fill!)
      (-obstacles 'score! (-menu 'score))
      (start-game-loop game-loop #t)
      dispatch)


    ;; Private

    (define (game-loop delta)
      (let ((percentage (scale-helper delta)))
        (-player    'render -display)
        (-obstacles 'render -display)
        (-player    'update! percentage -physics)
        (-obstacles 'update! percentage -physics)
        dispatch))

    (define (menu-loop delta)
      (-menu 'render -display)
      dispatch)

    (-log 'debug "initialized" kClass)

    dispatch))


;; (time-delta -> percentage)
(define (scale-helper delta) (/ delta 100))

(define kClass          'Game)
(define kPlayerPosition (MakePosition 200 200))
