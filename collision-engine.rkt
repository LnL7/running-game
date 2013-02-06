#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeCollisionEngine)



(define (MakeCollisionEngine #:log [-log (MakeLogger)])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((reset) player-reset)
        (else
          (-log 'fatal "method missing" msg kClass)))
      args))

  (define (player-reset velocity position end-jumping)
    (when (< (position 'x) kLeft)
      (position 'x! kLeft)
      (velocity 'horizontal! kBounceSpeed))

    (when (> (position 'x) kRight)
      (position 'x! kRight)
      (velocity 'horizontal! (- kBounceSpeed)))

    (when (< (position 'y) kBottom)
      (when end-jumping (end-jumping))
      (position 'y! kBottom)
      (velocity 'vertical! kBounceSpeed))

    (when (> (position 'y) kTop)
      (position 'y! kTop)
      (velocity 'vertical! 0)))


  ;; Private

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass       'CollisionEngine)
(define kLeft        0)
(define kRight       750)
(define kBottom      0)
(define kTop         500)
(define kBounceSpeed 10)
