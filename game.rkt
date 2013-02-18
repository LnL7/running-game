#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "menu.rkt"
         "world.rkt"
         "score.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-state     #f)
        (-menu      #f)
        (-world     #f)
        (-score     (MakeScore #:log -log))
        (-display   (MakeDisplay #:log -log))
        (-physics   (MakePhysics #:log -log))
        (-input     (MakeInput #:log -log)))
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

    (define (end)   (stop-game-loop))
    (define (start) (start-game-loop game-loop #t))

    (define (menu)
      (unless (eq? -state 'menu)
        (set! -state 'menu)
        (set! -world #f)
        (set! -menu  (MakeMenu -score #:log -log)))
      dispatch)

    (define (world)
      (unless (eq? -state 'world)
        (set! -state 'world)
        (set! -menu #f)
        (set! -world (MakeWorld #:log -log))
        (let ((player    (-world 'player))
              (obstacles (-world 'obstacles)))
          (-score 'end)
          (player 'physics -physics)
          (player 'input -input)
          (obstacles 'fill!)
          (obstacles 'score! -score)))
      dispatch)


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


    (-physics 'game! dispatch)
    (-input 'game! dispatch)
    (-input 'world)
    (menu)

    (-log 'debug "initialized" kClass)

    dispatch))


;; (time-delta -> percentage)
(define (scale-helper delta) (/ delta 100))

(define kClass 'Game)
