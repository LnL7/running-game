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
    (define (dispatch msg . args)
      (apply
        (case msg
          ((size)     get-size)
          ((position) get-position)
          ((velocity) get-velocity)
          ((jumping?) get-jumping?)
          ((jumping!) set-jumping!)
          ((bounce?)  get-bouncing?)
          ((bounce!)  set-bouncing!)
          ((collide!) collide!)
          ((reset!)   reset!)
          ((render)   render)
          ((update!)  update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (get-size)             -size)
    (define (get-position)         -position)
    (define (get-velocity)         -velocity)
    (define (get-jumping?)         -is-jumping)
    (define (set-jumping! jump)    (set! -is-jumping jump))
    (define (get-bouncing?)        -is-bouncing)
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

    (-log 'debug "initialized" kClass)
    dispatch))


(define kClass        'Player)
(define kNullVelocity (MakeVelocity 0 0))
(define kSize         50)
(define kDefaultPath  "resources/player1.png")
(define kCollidePath  "resources/player2.png")
(define kSlidePath    "resources/player3.png")
