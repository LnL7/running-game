#lang racket/base
(require "logger.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeObstacle
         MakeRandomObstacle)



(define (MakeRandomObstacle #:log [-log (MakeLogger)])
  (let* ((y      (random kMaxY))
         (x      (+ (random kMaxX) kMinX kOffsetX))
         (width  (+ (random kMaxSize) kMinSize))
         (height (+ (random kMaxSize) kMinSize))
         (pos    (MakePosition x y)))
    (set! kOffsetX x)
    (MakeObstacle pos width height #:log -log)))


(define (MakeObstacle position width height #:log [-log (MakeLogger)])
  (let ((-display-shape #f)
        (-physics-shape #f)
        (-position      position)
        (-width         width)
        (-height        height))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((position) get-position)
          ((render)   render)
          ((update!)  update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeRectangle -position -width -height kColor #:log -log)))
      (-display-shape 'render engine))

    (define (update! delta engine)
      (unless -physics-shape (set! -physics-shape (MakeRectangle -position -width -height #:log -log)))
      (-physics-shape 'update! delta engine kVelocity))

    (define (get-position) -position)


    ;; Private

    ; (-log 'debug "initialized" kClass -position -width -height)

    dispatch))



(define kClass    'Obstacle)
(define kVelocity (MakeVelocity -20 0))
(define kColor    "black")

(define kMinSize 25)
(define kMaxSize 300)
(define kMaxY    300)
(define kMaxX    500)
(define kMinX    0)
(define kOffsetX 800)
