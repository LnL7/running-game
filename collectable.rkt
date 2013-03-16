#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "velocity.rkt"
         "position.rkt"
         "shape.rkt")
(provide MakeRandomCollectable)


(define (MakeRandomCollectable offset speed x y #:log [-log (MakeLogger)])
  (y 'offset! (offset 'y))
  (let ((vel (MakeVelocity 0 (speed 'random) #:log -log))
        (pos (MakePosition (x 'random) (y 'random) #:log -log)))
    (offset 'y! (pos 'y))
    (MakeCollectable pos vel kSize #:log -log)))


(define (MakeCollectable position velocity size #:log [-log (MakeLogger)])
  (let ((-display-shape #f)
        (-update-shape  #f)
        (-size          size)
        (-velocity      velocity)
        (-position      position))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((position) get-position)
          ((render)   render)
          ((update!)  update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (get-position) -position)

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeEllipse -position -size -size kColor #:log -log)))
      (-display-shape 'render engine))

    (define (update! delta engine)
      (unless -update-shape (set! -update-shape (MakeRectangle -position -size -size #:log -log)))
      (-update-shape 'update! delta engine -velocity))

    ;; Private
    dispatch))


(define kClass 'Collectable)
(define kSize  10)
(define kColor "blue")
