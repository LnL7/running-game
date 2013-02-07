#lang racket/base
(require "logger.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeObstacle
         MakeRandomObstacle)



(define (MakeRandomObstacle offset #:log [-log (MakeLogger)])
  (let* ((y      (+ (random kMaxY) kMinY))
         (x      (+ (random kMaxX) (offset 'x) kMinX))
         (width  (+ (random kMaxSize) kMinSize))
         (height (+ (random kMaxSize) kMinSize))
         (pos    (MakePosition x y #:log -log)))
    (offset 'x! x)
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
(define kVelocity (MakeVelocity -10 0))
(define kColor    "black")

(define kMaxSize 200)
(define kMinSize 50)
(define kMaxX    800)
(define kMaxY    300)
(define kMinX    100)
(define kMinY    0)
