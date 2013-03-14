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
        (vel    (MakeVelocity (kSpeedRange 'random) 0 #:log -log))
        (pos    (MakePosition (kXRange 'random) (kYRange 'random) #:log -log)))
    (offset 'x! (pos 'x))
    (MakeObstacle pos vel width height #:log -log)))


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
      (unless -collide-proc  (set! -collide-proc  (engine 'collide fall! set-color! -width -height -position)))
      (-collide-proc delta)
      (-physics-shape 'update! delta engine -velocity))

    (define (get-position) -position)


    ;; Private
    (define (set-color! color)
      (-display-shape 'color! color))

    (define (fall!)
      (-velocity 'vertical! (kSpeedRange 'random)))


    ; (-log 'debug "initialized" kClass -position -width -height)

    dispatch))



(define kClass      'Obstacle)
(define kColor      "Black")
(define kSpeedRange (MakeRange -15 -5))
(define kSizeRange  (MakeRange 50  200))
(define kYRange     (MakeRange 0   300))
(define kXRange     (MakeRange 100 700))
