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
    (define (CollectableCollection msg . args)
      (case msg
        ((level!)  (apply set-level! args))
        ((render)  (apply render args))
        ((update!) (apply update! args))
        ((fill!)   (apply fill-collection! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch CollectableCollection)

    (define (set-level! level) (set! -level level))

    (define (render engine)
      (-collection 'each (lambda (collectable)
                           (collectable 'render engine))))

    (define (update! delta engine)
      (-collection 'each (lambda (collectable)
                           (collectable 'update! score! delta engine)
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
    (define (cleanup collectable) ;; Remove collectables if they are offscreen
      (when (and
              (eq? (-collection 'peek) collectable)
              (< (chain collectable 'position 'y) (-level 'collectable-cleanup-offset)))
        (-log 'debug "removing a" collectable)
        (-collection 'serve!)
        (fill-collection!)))

    (define (score!)
      ((-level 'score) 'add))

    (-log 'debug "initialized" dispatch)
    dispatch))


(define kSize         5)
(define kNullPosition (MakePosition 0 0))
