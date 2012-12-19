#lang racket/base
(require "lib/canvas.rkt")
(provide MakeDisplay)



(define (MakeDisplay)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape_rectangle)
        ((ellipse)   shape_ellipse)
        (else
          (error msg "method missing ~a" dispatch)))
      args))

  (define (shape_rectangle rectangle color)
    (lambda ()
      (let* ((width  (rectangle 'width))
             (height (rectangle 'height))
             (pos    (rectangle 'position))
             (x      (pos 'x))
             (y      (pos 'y)))
        (fill-rectangle! x y width height black))))

  (define (shape_ellipse ellipse color)
    (lambda ()
      (let* ((width  (ellipse 'width))
             (height (ellipse 'height))
             (pos    (ellipse 'position))
             (x      (pos 'x))
             (y      (pos 'y)))
        (fill-ellipse! x y width height black))))

  ;; Private

  dispatch)
