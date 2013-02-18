#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeCollisionEngine)



(define (MakeCollisionEngine #:log [-log (MakeLogger)])
  (let ((-game            #f)
        (-player-size     #f)
        (-player-velocity #f)
        (-player-position #f))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((game!)   set-game!)
          ((player!) set-player!)
          ((collide) obstacle-collide)
          ((reset)   player-reset)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (set-game! game)
      (set! -game game)
      dispatch)

    (define (set-player! velocity position size)
      (set! -player-size     size)
      (set! -player-velocity velocity)
      (set! -player-position position)
      dispatch)


    ;; (time-delta (-> player) position velocity -> dispatch)
    (define (player-reset delta end-jumping position velocity)
      (when (< (position 'x) kLeft)
        (position 'x! kLeft)
        (velocity 'horizontal! kBounceSpeed))
      (when (> (position 'x) kRight)
        (position 'x! kRight)
        (velocity 'horizontal! (- kBounceSpeed)))
      (when (< (position 'y) kBottom)
        (end-jumping)
        (position 'y! kBottom)
        (velocity 'vertical! kBounceSpeed))
      (when (> (position 'y) kTop)
        (position 'y! kTop)
        (velocity 'vertical! kNullSpeed))
      dispatch)

    ;; (time-delta (color -> shape) width height position)
    (define (obstacle-collide delta color! width height position)
      (let --iter ()
        (and
          -player-size
          -player-velocity
          -player-position
          (< (-player-position 'x) (+ (position 'x) width))
          (< (-player-position 'y) (+ (position 'y) height))
          (< (position 'x)         (+ (-player-position 'x) -player-size))
          (< (position 'y)         (+ (-player-position 'y) -player-size))
          (cond
            ((> (-player-velocity 'vertical) kCollideSpeed)  (end-game))
            ((> (-player-velocity 'horizontal) kBounceSpeed) (end-game))
            (else
              (color! "red")
              (if (< (position 'y) (-player-position 'y))
                (-player-velocity 'vertical! kBounceSpeed)
                (-player-velocity 'vertical! (- kBounceSpeed)))
              (-player-velocity 'horizontal! kNullSpeed)
              (move-player)
              (--iter))))
        dispatch))


    ;; Private

    (define (end-game)
      (if -game
        (-game 'end)
        (-log 'warn "game missing" 'end-game kClass))
      dispatch)

    (define (move-player)
      (-player-position 'move! -player-velocity)
      dispatch)

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass        'CollisionEngine)
(define kLeft         0)
(define kRight        750)
(define kBottom       0)
(define kTop          500)
(define kBounceSpeed  20)
(define kNullSpeed    0)
(define kCollideSpeed 8)
