#lang racket/base
(require "logger.rkt"
         "queue.rkt"
         "position.rkt"
         "velocity.rkt"
         "obstacle.rkt"
         "helpers.rkt")
(provide MakeObstacleCollection
         (rename-out [kSize kObstacleCollectionSize]))



(define (MakeObstacleCollection #:log [-log (MakeLogger)])
  (let ((-score      #f)
        (-offset     (MakePosition kGenerateOffset 0))
        (-collection (MakeQueue kSize #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((score!)  set-score!)
          ((render)  render)
          ((update!) update!)
          ((fill!)   fill-collection!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (render engine)
      (-collection 'each (lambda (obstacle)
                           (obstacle 'render engine))))

    (define (update! delta engine)
      (-collection 'each (lambda (obstacle)
                           (obstacle 'update! delta engine)
                           (cleanup obstacle))))

    (define (fill-collection!)
      (if (-collection 'full?)
        (-offset 'x! kGenerateOffset)
        (begin
          (-collection 'enqueue! (MakeRandomObstacle -offset #:log -log))
          (fill-collection!))))

    (define (set-score! score)
      (set! -score score))


    ;; Private

    (define (cleanup obstacle)
      (when (and
              (eq? (-collection 'peek) obstacle)
              (< (chain obstacle 'position 'x) kCleanupOffset))
        (-log 'debug "removing an" 'Obstacle)
        (when -score (-score 'add))
        (-collection 'serve!)
        (fill-collection!)))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass          'ObstacleCollection)
(define kSize           5)
(define kCleanupOffset  -300)
(define kGenerateOffset 800)
