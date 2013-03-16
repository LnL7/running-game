#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "queue.rkt"
         "position.rkt"
         "collectable.rkt")
(provide MakeCollectableCollection)


(define (MakeCollectableCollection #:log [-log (MakeLogger)])
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
      (-collection 'each (lambda (collectable)
                           (collectable 'render engine))))

    (define (update! delta engine)
      (-collection 'each (lambda (collectable)
                           (collectable 'update! delta engine)
                           (cleanup collectable))))

    (define (fill-collection!)
      (-offset 'y! (-level 'collectable-generate-offset))
      (let ((speed  (-level 'collectable-speed))
            (x      (-level 'collectable-x))
            (y      (-level 'collectable-y)))
        (let --iter ()
          (unless (-collection 'full?)
            (-collection 'enqueue! (MakeRandomCollectable -offset speed x y #:log -log))
            (--iter)))))

    ;; Private
    (define (cleanup collectable)
      (when (and
              (eq? (-collection 'peek) collectable)
              (< (chain collectable 'position 'y) (-level 'collectable-cleanup-offset)))
        (-log 'debug "removing a" 'Collectable)
        (-collection 'serve!)
        (fill-collection!)))

    dispatch))


(define kClass        'CollectableCollection)
(define kSize         5)
(define kNullPosition (MakePosition 0 0))
