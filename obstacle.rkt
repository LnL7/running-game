#lang racket/base
(require "logger.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeObstacle
         MakeRandomObstacle)


(define (MakeRandomObstacle offset speed size x y #:log [-log (MakeLogger)])
  (x 'offset! (offset 'x))
  (let ((width  (size 'random))
        (height (size 'random))
        (vel    (MakeVelocity (speed 'random) 0 #:log -log))
        (pos    (MakePosition (x 'random) (y 'random) #:log -log)))
    (offset 'x! (pos 'x))
    (MakeObstacle pos vel width height #:log -log)))


(define (MakeObstacle -position -velocity -width -height #:log [-log (MakeLogger)])
  (let ((-display-shape #f)
        (-physics-shape #f)
        (-collide-proc  #f))
    (define (Obstacle msg . args)
      (case msg
        ((position) -position)
        ((render)   (apply render args))
        ((update!)  (apply update! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Obstacle)

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeRectangle -position -width -height kColor #:log -log)))
      (-display-shape 'render engine))

    (define (update! delta engine)
      (unless -physics-shape (set! -physics-shape (MakeRectangle -position -width -height #:log -log)))
      (unless -collide-proc  (set! -collide-proc  (engine 'collide collide! -width -height -position)))
      (-collide-proc delta)
      (-physics-shape 'update! delta engine -velocity))

    ;; Private
    (define (collide! menu!)
      (cond
        (menu! (menu!)) ;; exit to menu on hard collision
        (else ;; soft collision
          (-velocity 'vertical! (-velocity 'horizontal)) ;; Start falling
          (-display-shape 'color! kCollideColor)
          #t))) ;; check collisions again

    ; (-log 'debug "initialized" dispatch)
    dispatch))


(define kColor        "black")
(define kCollideColor "gray")
