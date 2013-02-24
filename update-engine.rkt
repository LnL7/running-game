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
      (vel 'scale! delta)
      (pos 'move! vel)
    dispatch))

  (define (gravity delta friction gravity-velocity position velocity)
    (let ((vel     (velocity 'copy))
          (gravity (gravity-velocity 'copy)))
      (velocity 'add! (vel 'scale! (/ delta friction)))
      (velocity 'add! (gravity 'scale! delta))
      dispatch))


  ;; Private

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass 'UpdateEngine)
