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
        ((collide) collide)
        ((reset)   player-reset)
        (else
          (-log 'fatal "method missing" msg kClass)))
      args))

  (define (collide delta bounce-range game player collide! width height position)
    (define (menu!) (game 'menu) #f)
    (let ((player-size     (player 'size))
          (player-position (player 'position))
          (player-velocity (player 'velocity))
          (bounce          (bounce-range 'random))
          (max-bounce      (bounce-range 'to)))
      (let --iter ()
        (when (collision? player-size player-position width height position)
          (cond
            ((> (player-velocity 'vertical) kCollideSpeed) (collide! menu!)) ;; hard collision
            ((> (player-velocity 'horizontal) max-bounce)  (collide! menu!)) ;; hard collision
            ((collide! #f) ;; soft collision
             (player 'collide!)
             (if (< (position 'y) (player-position 'y))
               (player-velocity 'vertical! bounce)
               (player-velocity 'vertical! (- bounce)))
             (player-velocity 'horizontal! kNullSpeed)
             (player-position 'move! player-velocity)
             (--iter)))))))

  (define (player-reset delta bounce-range player position velocity)
    (let ((bounce (if (player 'bounce?) (bounce-range 'random) 0)))
      (when (< (position 'x) kLeft)
        (position 'x! kLeft)
        (velocity 'horizontal! bounce))
      (when (> (position 'x) kRight)
        (position 'x! kRight)
        (velocity 'horizontal! (- bounce)))
      (when (< (position 'y) kBottom)
        (player 'reset!)
        (position 'y! kBottom)
        (velocity 'vertical! bounce))
      (when (> (position 'y) kTop)
        (position 'y! kTop)
        (velocity 'vertical! kNullSpeed))
      dispatch))

  ;; Private
  (define (collision? player-size player-position width height position)
    (and
      (< (player-position 'x) (+ (position 'x) width))
      (< (player-position 'y) (+ (position 'y) height))
      (< (position 'x)        (+ (player-position 'x) player-size))
      (< (position 'y)        (+ (player-position 'y) player-size))))

  (-log 'debug "initialized" kClass)
  dispatch)


(define kClass        'CollisionEngine)
(define kLeft         0)
(define kRight        750)
(define kBottom       0)
(define kTop          500)
(define kNullSpeed    0)
(define kCollideSpeed 8)
