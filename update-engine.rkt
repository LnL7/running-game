#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakeUpdateEngine)



(define (MakeUpdateEngine #:log [-log (MakeLogger)])
  (define (UpdateEngine msg . args)
    (case msg
      ((rectangle) (apply shape-rectangle args))
      ((gravity)   (apply gravity args))
      (else
        (-log 'fatal "method missing" msg dispatch))))
  (define dispatch UpdateEngine)

  (define (shape-rectangle delta rectangle velocity)
    (let ((pos (rectangle 'position))
          (vel (velocity 'copy)))
      (vel 'scale! delta)
      (pos 'move! vel)
    dispatch))

  (define (gravity delta mass friction gravity position velocity)
    (let ((vel     (velocity 'copy))
          (gravity (gravity 'copy)))
      (velocity 'add! (vel 'scale! (/ delta friction)))
      (velocity 'add! (gravity 'scale! (* mass delta)))
      dispatch))

  ;; Private
  (-log 'debug "initialized" dispatch)
  dispatch)
