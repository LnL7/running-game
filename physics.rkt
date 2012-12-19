#lang racket/base
(provide MakePhysics)



(define (MakePhysics)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape_rectangle)
        (else
          (error msg "method missing ~a" dispatch)))
      args))

  (define (shape_rectangle rectangle)
    (lambda (delta)
      (let ((pos (rectangle 'position))
            (vel ((rectangle 'velocity) 'copy)))
        (vel 'scale! (/ delta 100))
        (pos 'move! vel))))


  dispatch)
