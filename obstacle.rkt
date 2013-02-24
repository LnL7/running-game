#lang racket/base
(require "logger.rkt"
         "range.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeObstacle
         MakeRandomObstacle)



(define (MakeRandomObstacle offset #:log [-log (MakeLogger)])
  (kXRange 'offset! (offset 'x))
  (let ((width  (kSizeRange 'random))
        (height (kSizeRange 'random))
        (pos    (MakePosition (kXRange 'random) (kYRange 'random) #:log -log)))
    (offset 'x! (pos 'x))
    (MakeObstacle pos kVelocity width height #:log -log)))


(define (MakeObstacle position velocity width height #:log [-log (MakeLogger)])
  (let ((-display-shape #f)
        (-physics-shape #f)
        (-collide-proc  #f)
        (-position      position)
        (-velocity      velocity)
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
      (unless -collide-proc  (set! -collide-proc  (engine 'collide set-color! -width -height -position)))
      (-collide-proc delta)
      (-physics-shape 'update! delta engine -velocity))

    (define (get-position) -position)


    ;; Private

    (define (set-color! color)
      (-display-shape 'color! color))


    ; (-log 'debug "initialized" kClass -position -width -height)

    dispatch))



(define kClass    'Obstacle)
(define kColor    "black")
(define kVelocity  (MakeVelocity -10 0))
(define kSizeRange (MakeRange 50  200))
(define kYRange    (MakeRange 0   300))
(define kXRange    (MakeRange 100 700))
