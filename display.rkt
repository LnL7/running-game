#lang racket/base
(require "lib/canvas.rkt"
         "helpers.rkt")
(provide MakeDisplay)



(define (MakeDisplay)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape_rectangle)
        ((ellipse)   shape_ellipse)
        (else
          (method_missing msg dispatch)))
      args))

  (define (shape_rectangle rectangle color_name)
    (lambda ()
      (let* ((color  (find-color color_name))
             (width  (rectangle 'width))
             (height (rectangle 'height))
             (pos    (rectangle 'position))
             (x      (pos 'x))
             (y      (pos 'y)))
        (fill-rectangle! x y width height color))))

  (define (shape_ellipse ellipse color_name)
    (lambda ()
      (let* ((color  (find-color color_name))
             (width  (ellipse 'width))
             (height (ellipse 'height))
             (pos    (ellipse 'position))
             (x      (pos 'x))
             (y      (pos 'y)))
        (fill-ellipse! x y width height color))))

  ;; Private

  dispatch)
