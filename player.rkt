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
        (-position      position)
        (-velocity      kNullVelocity))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((input)   input)
          ((render)  render)
          ((update!) update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (input engine)
      (engine 'strafe -velocity)
      (engine 'jump get-is-jumping? start-jumping -velocity))

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeImage -position kSize kSize kPath #:log -log)))
      (-display-shape 'render engine))

    (define (update! engine)
      (unless -physics-shape (set! -physics-shape (MakeRectangle -position kSize kSize #:log -log)))
      (lambda args
        (apply (engine 'gravity -velocity -position end-jumping) args)
        (apply (-physics-shape 'move! engine -velocity) args)))

    ;; Private

    (define (get-is-jumping?) -is-jumping)

    (define (end-jumping)
      (-log 'debug "end jumping, Player")
      (set! -is-jumping #f))

    (define (start-jumping)
      (-log 'debug "start jumping, Player")
      (set! -is-jumping #t))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass        'Player)
(define kNullVelocity (MakeVelocity 0 0))
(define kSize          50)
(define kPath          "resources/player.png")
