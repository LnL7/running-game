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

    (define (player-reset delta velocity position end-jumping)
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
        (velocity 'vertical! 0)))

    (define (obstacle-collide delta position width height set-color)
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
            ((> (-player-velocity 'vertical) 8)   (end-game))
            ((> (-player-velocity 'horizontal) 20) (end-game))
            (else
              (set-color "red")
              (if (< (position 'y) (-player-position 'y))
                (-player-velocity 'vertical! 20)
                (-player-velocity 'vertical! -20))
              (-player-velocity 'horizontal! 0)
              (move-player)
              (--iter))))))


    (define (set-game! game)
      (set! -game game))

    (define (set-player! velocity position size)
      (set! -player-size     size)
      (set! -player-velocity velocity)
      (set! -player-position position))


    ;; Private

    (define (end-game)
      (if -game
        (-game 'end)
        (-log 'warn "game missing" 'end-game kClass)))

    (define (move-player)
      (-player-position 'move! -player-velocity))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass       'CollisionEngine)
(define kLeft        0)
(define kRight       750)
(define kBottom      0)
(define kTop         500)
(define kBounceSpeed 20)
