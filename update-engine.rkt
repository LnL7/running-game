#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeUpdateEngine)



(define (MakeUpdateEngine #:log [-log (MakeLogger)])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape-rectangle)
        ((gravity)   gravity)
        (else
          (-log 'fatal "method missing" msg kClass)))
      args))

  ;; (percentage shape-rectangle velocity -> self)
  (define (shape-rectangle delta rectangle velocity)
    (let ((pos (rectangle 'position))
          (vel (velocity 'copy)))
      (vel 'scale! delta)
      (pos 'move! vel)
    dispatch))

  ;; (percentage position velocity -> self)
  (define (gravity delta position velocity)
    (let ((vel     (velocity 'copy))
          (gravity (kGravityVelocity 'copy)))
      (velocity 'add! (vel 'scale! (/ delta kFriction)))
      (velocity 'add! (gravity 'scale! delta))
      dispatch))


  ;; Private

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass           'UpdateEngine)
(define kGravityVelocity (MakeVelocity 0 (* -0.94 4)))
(define kFriction        -10)
