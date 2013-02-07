#lang racket/base
(require "logger.rkt"
         "queue.rkt"
         "velocity.rkt"
         "obstacle.rkt"
         "helpers.rkt")
(provide MakeObstacleCollection)



(define (MakeObstacleCollection #:log [-log (MakeLogger)])
  (let ((-collection (MakeQueue kSize #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((render)  render)
          ((update!) update!)
          ((fill!)   fill-collection!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (render . args)
      (-collection 'each (lambda (obstacle)
                           (apply obstacle 'render args))))

    (define (update! delta . args)
      (-collection 'each (lambda (obstacle)
                           (apply obstacle 'update! delta args)
                           (cleanup obstacle))))

    (define (fill-collection!)
      (unless (-collection 'full?)
        (-collection 'enqueue! (MakeRandomObstacle #:log -log))
        (fill-collection!)))


    ;; Private

    (define (cleanup obstacle)
      (when (and
              (eq? (-collection 'peek) obstacle)
              (< (chain obstacle 'position 'x) kCleanupOffset))
        ; (-log 'debug "removing an" 'Obstacle)
        (-collection 'serve!)
        (fill-collection!)))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass         'ObstacleCollection)
(define kSize          5)
(define kCleanupOffset -300)
