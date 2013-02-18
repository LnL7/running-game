#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "menu.rkt"
         "world.rkt"
         "player.rkt"
         "obstacle-collection.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-state     'world)
        (-menu      #f)
        (-world     #f)
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
          ((menu)  menu)
          ((world) world)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (end) (stop-game-loop))

    (define (start)
      (set! -menu      (MakeMenu #:log -log))
      (set! -world     (MakeWorld #:log -log))
      (set! -display   (MakeDisplay #:log -log))
      (set! -physics   (MakePhysics #:log -log))
      (set! -input     (MakeInput #:log -log))

      (-physics 'game! dispatch)
      (dispatch -state)

      (start-game-loop game-loop #t)
      dispatch)

    (define (menu)
      (set! -state 'menu)
      (set! -world #f)
      dispatch)

    (define (world)
      (set! -state 'world)
      (set! -world (MakeWorld #:log -log))
      (let ((score     (-menu 'score))
            (player    (-world 'player))
            (obstacles (-world 'obstacles)))
        (score 'end)
        (player 'physics -physics)
        (player 'input -input)
        (obstacles 'fill!)
        (obstacles 'score! score)
        dispatch))


    ;; Private

    (define (game-loop delta)
      (let ((percentage (scale-helper delta)))
        (case -state
          ((menu)  (menu-loop delta))
          ((world) (world-loop delta))
          (else
            (-log 'fatal "invalid state" -state kClass)))))

    (define (world-loop delta)
      (-world 'render -display)
      (-world 'update! (scale-helper delta) -physics))

    (define (menu-loop delta)
      (-menu 'render -display))

    (-log 'debug "initialized" kClass)

    dispatch))


;; (time-delta -> percentage)
(define (scale-helper delta) (/ delta 100))

(define kClass          'Game)
