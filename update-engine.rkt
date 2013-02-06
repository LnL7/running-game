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

  (define (shape-rectangle delta rectangle velocity)
    (let ((pos (rectangle 'position))
          (vel (velocity 'copy)))
      (vel 'scale! (/ delta 100))
      (pos 'move! vel)))

  (define (gravity delta velocity position end-jumping)
    (let ((vel     (velocity 'copy))
          (gravity (kGravityVelocity 'copy)))
      (velocity 'add! (vel 'scale! (/ delta -1000)))
      (velocity 'add! (gravity 'scale! (/ delta 100)))))


  ;; Private

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass           'UpdateEngine)
(define kGravityVelocity (MakeVelocity 0 (* -0.94 4)))
