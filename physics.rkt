#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "helpers.rkt")
(provide MakePhysics)



(define (MakePhysics #:log [-log (MakeLogger)])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape-rectangle)
        ((gravity)   velocity-gravity)
        (else
          (-log 'fatal "method missing" msg kClass)))
      args))

  (define (shape-rectangle rectangle velocity)
    (lambda (delta)
      (let ((pos (rectangle 'position))
            (vel (velocity 'copy)))
        (vel 'scale! (/ delta 100))
        (pos 'move! vel))))

  (define (velocity-gravity velocity position end-jumping)
    (position-reset position velocity end-jumping)
    (lambda (delta)
      (let ((vel     (velocity 'copy))
            (gravity (kGravityVelocity 'copy)))
        (velocity 'add! (vel 'scale! (/ delta -1000)))
        (velocity 'add! (gravity 'scale! (/ delta 100))))))


  ;; Private

  (define (position-reset position velocity end-jumping)
    (when (< (position 'x) 0)
      (position 'x! 0)
      (velocity 'horizontal! 1))

    (when (> (position 'x) 750)
      (position 'x! 750)
      (velocity 'horizontal! -1))

    (when (< (position 'y) kFloorHeight)
      (when end-jumping (end-jumping))
      (position 'y! kFloorHeight)
      (velocity 'vertical! kBounceSpeed))

    (when (> (position 'y) 550)
      (position 'y! 550)
      (velocity 'vertical! 0)))

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass           'Physics)
(define kFloorHeight     0)
(define kBounceSpeed     10)
(define kGravityVelocity (MakeVelocity 0 (* -0.94 4)))
