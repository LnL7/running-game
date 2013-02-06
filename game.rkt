#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "player.rkt"
         "obstacle.rkt"
         "display.rkt"
         "physics.rkt"
         "input.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-player    #f)
        (-obstacles '())
        (-display   (MakeDisplay #:log -log))
        (-physics   (MakePhysics #:log -log))
        (-input     (MakeInput #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((player)    start-player)
          ((obstacles) start-obstacles)
          ((loop)      start-loop)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (start-player)
      (let ((position (MakePosition 200 200 #:log -log)))
        (set! -player (MakePlayer position #:log -log))))

    (define (start-obstacles)
      (let ((obstacles (list
                         (MakeObstacle (MakePosition 600 100) 300 100 #:log -log)
                         (MakeObstacle (MakePosition 1000 300) 100 200 #:log -log)
                         (MakeObstacle (MakePosition 1500 000) 200 300 #:log -log))))
        (set! -obstacles obstacles)))

    (define (start-loop)
      (if -player
        (-player 'input -input)
        (-log 'warn "no player in Game"))

      (start-game-loop
        (lambda (delta)
          (let ((objects (cons -player -obstacles)))
            (for-each
              (lambda (obj) ((object-loop obj) delta))
              objects)))
        #t))

    ;; Private

    (define (object-loop obj)
      (if obj
        (lambda (delta)
          ((obj 'render -display)  delta)
          ((obj 'update! -physics) delta))
        (lambda (delta) #f)))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Game)
