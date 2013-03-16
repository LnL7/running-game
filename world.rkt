#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "player.rkt"
         "obstacle-collection.rkt"
         "collectable-collection.rkt")
(provide MakeWorld)


(define (MakeWorld #:log [-log (MakeLogger)])
  (let ((-player       (MakePlayer (kPlayerPosition 'copy) #:log -log))
        (-obstacles    (MakeObstacleCollection #:log -log))
        (-collectables (MakeCollectableCollection #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((player)       get-player)
          ((obstacles)    get-obstacles)
          ((collectables) get-collectables)
          ((render)       render)
          ((update!)      update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties
    (define (get-player)       -player)
    (define (get-obstacles)    -obstacles)
    (define (get-collectables) -collectables)

    (define (render . args)
      (let ((msg 'render))
        (apply -player msg args)
        (apply -obstacles msg args)
        (apply -collectables msg args)))

    (define (update! . args)
      (let ((msg 'update!))
        (apply -player msg args)
        (apply -obstacles msg args)
        (apply -collectables msg args)))

    ;; Private
    dispatch))


(define kClass          'World)
(define kPlayerPosition (MakePosition 200 200))
