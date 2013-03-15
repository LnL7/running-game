#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "range.rkt"
         "helpers.rkt")
(provide MakeCollisionEngine)



(define (MakeCollisionEngine #:log [-log (MakeLogger)])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((collide) obstacle-collide)
        ((reset)   player-reset)
        (else
          (-log 'fatal "method missing" msg kClass)))
      args))

  ;; Properties


  ;; (time-delta (-> player) position velocity -> dispatch)
  (define (player-reset delta bounce-range end-jumping position velocity)
    (let ((bounce (bounce-range 'random)))
      (when (< (position 'x) kLeft)
        (position 'x! kLeft)
        (velocity 'horizontal! bounce))
      (when (> (position 'x) kRight)
        (position 'x! kRight)
        (velocity 'horizontal! (- bounce)))
      (when (< (position 'y) kBottom)
        (end-jumping)
        (position 'y! kBottom)
        (velocity 'vertical! bounce))
      (when (> (position 'y) kTop)
        (position 'y! kTop)
        (velocity 'vertical! kNullSpeed))
      dispatch))

  ;; (time-delta game player (color -> shape) width height position)
  (define (obstacle-collide delta bounce-range game player fall! color! width height position)
    (let ((player-size     (player 'size))
          (player-position (player 'position))
          (player-velocity (player 'velocity))
          (bounce          (bounce-range 'random))
          (max-bounce      (bounce-range 'to)))
      (let --iter ()
        (and
          (< (player-position 'x) (+ (position 'x) width))
          (< (player-position 'y) (+ (position 'y) height))
          (< (position 'x)        (+ (player-position 'x) player-size))
          (< (position 'y)        (+ (player-position 'y) player-size))
          (cond
            ((> (player-velocity 'vertical) kCollideSpeed) (game 'menu))
            ((> (player-velocity 'horizontal) max-bounce)  (game 'menu))
            (else
              (fall!)
              (color! "red")
              (if (< (position 'y) (player-position 'y))
                (player-velocity 'vertical! bounce)
                (player-velocity 'vertical! (- bounce)))
              (player-velocity 'horizontal! kNullSpeed)
              (player-position 'move! player-velocity)
              (--iter)))))
      dispatch))


  ;; Private
  (-log 'debug "initialized" kClass)
  dispatch)


(define kClass        'CollisionEngine)
(define kLeft         0)
(define kRight        750)
(define kBottom       0)
(define kTop          500)
(define kNullSpeed    0)
(define kCollideSpeed 8)
