#lang racket/base
(require "lib/canvas.rkt"
         "logger.rkt"
         "menu.rkt"
         "world.rkt"
         "level.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)


(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-state   #f)
        (-menu    #f)
        (-world   #f)
        (-level   kDefaultLevelIndex)
        (-display (MakeDisplay #:log -log))
        (-physics (MakePhysics #:log -log))
        (-input   (MakeInput #:log -log))
        (-levels  (vector
                    (MakeDefaultLevel #:log -log)
                    (MakeSpeedyLevel #:log -log))))
    (define (Game msg . args)
      (case msg
        ((end)   (apply end args))
        ((start) (apply start args))
        ((menu)  (apply menu args))
        ((world) (apply world args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Game)

    (define (end)   (stop-game-loop))
    (define (start) (start-game-loop game-loop #t))

    (define (menu)
      (unless (eq? -state 'menu)
        (let* ((level (vector-ref -levels -level))
               (score (level 'score)))
          (next-level!)
          (set! -state 'menu)
          (set! -world #f)
          (set! -menu  (MakeMenu score #:log -log))))
      dispatch)

    (define (world)
      (unless (eq? -state 'world)
        (set! -state 'world)
        (set! -menu #f)
        (set! -world (MakeWorld #:log -log))
        (let* ((level        (vector-ref -levels -level))
               (score        (level 'score))
               (player       (-world 'player))
               (obstacles    (-world 'obstacles))
               (collectables (-world 'collectables)))
          (score 'end)
          (-physics 'level! level)
          (-physics 'player! player)
          (-input 'level! level)
          (-input 'strafe player)
          (-input 'jump player)
          (-input 'slide player)
          (obstacles 'level! level)
          (obstacles 'fill!)
          (collectables 'level! level)
          (collectables 'fill!)))
      dispatch)

    ;; Private
    (define (game-loop delta)
      (let ((percentage (scale-helper delta)))
        (case -state
          ((menu)  (menu-loop delta))
          ((world) (world-loop delta))
          (else
            (-log 'fatal "invalid state" -state dispatch)))))

    (define (world-loop delta)
      (-world 'render -display)
      (-world 'update! (scale-helper delta) -physics))

    (define (menu-loop delta)
      (-menu 'render -display))

    (define (next-level!)
      (set! -level (modulo
                     (+ -level 1)
                     (vector-length -levels))))

    (-physics 'game! dispatch)
    (-input 'game! dispatch)
    (-input 'world)
    (world)
    (-log 'debug "initialized" dispatch)
    dispatch))


;; (time-delta -> percentage)
(define (scale-helper delta) (/ delta 100))

(define kDefaultLevelIndex 0)
