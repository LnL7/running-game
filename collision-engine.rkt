#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeCollisionEngine)



(define (MakeCollisionEngine #:log [-log (MakeLogger)])
  (let ((-player-size     #f)
        (-player-velocity #f)
        (-player-position #f))
    (define (dispatch msg . args)
      (apply
        (case msg
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
      (and
        -player-size
        -player-velocity
        -player-position
        (< (-player-position 'x) (+ (position 'x) width))
        (< (-player-position 'y) (+ (position 'y) height))
        (< (position 'x)         (+ (-player-position 'x) -player-size))
        (< (position 'y)         (+ (-player-position 'y) -player-size))
        (begin
          (set-color "red")
          #t)))


    (define (set-player! velocity position size)
      (set! -player-size     size)
      (set! -player-velocity velocity)
      (set! -player-position position))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass       'CollisionEngine)
(define kLeft        0)
(define kRight       750)
(define kBottom      0)
(define kTop         500)
(define kBounceSpeed 20)
