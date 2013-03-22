#lang racket/base
(require "logger.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakePlayer)


(define (MakePlayer position #:log [-log (MakeLogger)])
  (let ((-is-jumping    #t)
        (-is-bouncing   #t)
        (-display-shape #f)
        (-physics-shape #f)
        (-gravity-proc  #f)
        (-reset-proc    #f)
        (-size          kSize)
        (-position      position)
        (-velocity      (kNullVelocity 'copy)))
    (define (Player msg . args)
      (case msg
        ((size)     -size)
        ((position) -position)
        ((velocity) -velocity)
        ((jumping?) -is-jumping)
        ((bounce?)  -is-bouncing)
        ((jumping!) (apply set-jumping! args))
        ((bounce!)  (apply set-bouncing! args))
        ((collide!) (apply collide! args))
        ((reset!)   (apply reset! args))
        ((render)   (apply render args))
        ((update!)  (apply update! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Player)

    (define (set-jumping! jump)    (set! -is-jumping jump))
    (define (set-bouncing! bounce) (set! -is-bouncing bounce))

    (define (collide!)
      (when -display-shape
        (-display-shape 'path! kCollidePath)))

    (define (reset!)
      (set! -is-jumping #f)
      (when -display-shape
        (if -is-bouncing
          (-display-shape 'path! kDefaultPath)
          (-display-shape 'path! kSlidePath))))

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeImage -position kSize kSize kDefaultPath #:log -log)))
      (-display-shape 'render engine))

    (define (update! delta engine)
      (unless -physics-shape  (set! -physics-shape (MakeRectangle -position kSize kSize #:log -log)))
      (unless -reset-proc     (set! -reset-proc    (engine 'reset dispatch -position -velocity)))
      (unless -gravity-proc   (set! -gravity-proc  (engine 'gravity 'player-mass -position -velocity)))
      (-reset-proc delta)
      (-gravity-proc delta)
      (-physics-shape 'update! delta engine -velocity))

    ;; Private
    (define (get-is-jumping?) -is-jumping)
    (define (start-jumping) (set! -is-jumping #t))

    (-log 'debug "initialized" dispatch)
    dispatch))


(define kNullVelocity (MakeVelocity 0 0))
(define kSize         50)
(define kDefaultPath  "resources/player1.png")
(define kCollidePath  "resources/player2.png")
(define kSlidePath    "resources/player3.png")
