#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "score.rkt"
         "player.rkt"
         "obstacle-collection.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-score     #f)
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
      (set! -score     (MakeScore #:log -log))
      (set! -player    (MakePlayer kPlayerPosition #:log -log))
      (set! -obstacles (MakeObstacleCollection #:log -log))
      (set! -display   (MakeDisplay #:log -log))
      (set! -physics   (MakePhysics #:log -log))
      (set! -input     (MakeInput #:log -log))

      (-physics 'game! dispatch)
      (-player 'physics -physics)
      (-player 'input -input)
      (-obstacles 'score! -score)
      (-obstacles 'fill!)
      (start-game-loop game-loop #t))


    ;; Private

    (define (game-loop delta)
      (-player    'render -display)
      (-obstacles 'render -display)
      (-player    'update! delta -physics)
      (-obstacles 'update! delta -physics))

    (define (menu-loop delta)
      (let ((score (number->string (-score 'current))))
        (draw-text! 250 400 "Game Over!" "blue" (make-font #:size 64 #:face "Junction"))
        (draw-text! 400 300 score "blue" (make-font #:size 32 #:face "Junction"))))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass          'Game)
(define kPlayerPosition (MakePosition 200 200))
