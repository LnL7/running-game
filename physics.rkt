#lang racket/base
(require "helpers.rkt")
(provide MakePhysics)



(define (MakePhysics)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape_rectangle)
        (else
          (method_missing msg dispatch)))
      args))

  (define (shape_rectangle rectangle)
    (lambda (delta)
      (let ((pos (rectangle 'position))
            (vel ((rectangle 'velocity) 'copy)))
        (vel 'scale! (/ delta 100))
        (pos 'move! vel))))


  dispatch)
