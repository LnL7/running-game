#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "player.rkt"
         "obstacle-collection.rkt")
(provide MakeWorld)



(define (MakeWorld #:log [-log (MakeLogger)])
  (let ((-player    (MakePlayer (kPlayerPosition 'copy) #:log -log))
        (-obstacles (MakeObstacleCollection #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((player)    get-player)
          ((obstacles) get-obstacles)
          ((render)    render)
          ((update!)   update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (get-player)    -player)
    (define (get-obstacles) -obstacles)


    (define (render . args)
      (apply -player 'render args)
      (apply -obstacles 'render args)
      dispatch)

    (define (update! . args)
      (apply -player 'update! args)
      (apply -obstacles 'update! args)
      dispatch)


    ;; Private

    dispatch))


(define kClass          'World)
(define kPlayerPosition (MakePosition 200 200))
