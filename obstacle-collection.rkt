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
  (let ((-level      #f)
        (-offset     kNullPosition)
        (-collection (MakeQueue kSize #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((level!)  set-level!)
          ((render)  render)
          ((update!) update!)
          ((fill!)   fill-collection!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (set-level! level) (set! -level level))

    (define (render engine)
      (-collection 'each (lambda (obstacle)
                           (obstacle 'render engine))))

    (define (update! delta engine)
      (-collection 'each (lambda (obstacle)
                           (obstacle 'update! delta engine)
                           (cleanup obstacle))))

    (define (fill-collection!)
      (-offset 'x! (-level 'obstacle-generate-offset))
      (let ((speed (-level 'obstacle-speed))
            (size  (-level 'obstacle-size))
            (x     (-level 'obstacle-x))
            (y     (-level 'obstacle-y)))
        (let --iter ()
          (unless (-collection 'full?)
            (-collection 'enqueue! (MakeRandomObstacle -offset speed size x y #:log -log))
            (--iter)))))


    ;; Private
    (define (cleanup obstacle)
      (when (and
              (eq? (-collection 'peek) obstacle)
              (< (chain obstacle 'position 'x) (-level 'obstacle-cleanup-offset)))
        (-log 'debug "removing an" 'Obstacle)
        (when -level ((-level 'score) 'add))
        (-collection 'serve!)
        (fill-collection!)))

    ;; Private
    (-log 'debug "initialized" kClass)
    dispatch))


(define kClass        'ObstacleCollection)
(define kSize         5)
(define kNullPosition (MakePosition 0 0))
