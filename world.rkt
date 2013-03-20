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
    (define (World msg . args)
      (case msg
        ((player)       -player)
        ((obstacles)    -obstacles)
        ((collectables) -collectables)
        ((render)       (apply render args))
        ((update!)      (apply update! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch World)

    ;; Properties
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
    (-log 'debug "initialized" dispatch)
    dispatch))


(define kPlayerPosition (MakePosition 200 200))
