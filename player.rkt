#lang racket/base
(require "logger.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakePlayer)



(define (MakePlayer position #:log [-log (MakeLogger)])
  (let ((-is-jumping    #t)
        (-display-shape #f)
        (-physics-shape #f)
        (-gravity-proc  #f)
        (-reset-proc    #f)
        (-size          kSize)
        (-position      position)
        (-velocity      (kNullVelocity 'copy)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((physics) physics)
          ((input)   input)
          ((render)  render)
          ((update!) update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (physics engine)
      (engine 'player! -velocity -position -size))

    (define (input engine)
      (engine 'strafe -velocity)
      (engine 'jump get-is-jumping? start-jumping -velocity))

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeImage -position kSize kSize kPath #:log -log)))
      (-display-shape 'render engine))

    (define (update! delta engine)
      (unless -physics-shape  (set! -physics-shape (MakeRectangle -position kSize kSize #:log -log)))
      (unless -reset-proc     (set! -reset-proc    (engine 'reset end-jumping -position -velocity)))
      (unless -gravity-proc   (set! -gravity-proc  (engine 'gravity -position -velocity)))
      (-reset-proc delta)
      (-gravity-proc delta)
      (-physics-shape 'update! delta engine -velocity))


    ;; Private

    (define (get-is-jumping?) -is-jumping)
    (define (end-jumping) (set! -is-jumping #f))
    (define (start-jumping) (set! -is-jumping #t))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass        'Player)
(define kNullVelocity (MakeVelocity 0 0))
(define kSize          50)
(define kPath          "resources/player.png")
