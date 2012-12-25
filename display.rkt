#lang racket/base
(require "lib/canvas.rkt"
         "helpers.rkt")
(provide MakeDisplay)



(define (MakeDisplay)
    (define (dispatch msg . args)
      (apply
        (case msg
          ((ellipse)   shape_ellipse)
          ((rectangle) shape_rectangle)
          ((image)     shape_image)
          (else
            (method_missing msg dispatch)))
        args))

    (define (shape_ellipse ellipse color_name)
      (let ((color  (find-color color_name))
            (width  (ellipse 'width))
            (height (ellipse 'height))
            (pos    (ellipse 'position)))
        (lambda (delta)
          (let ((x (pos 'x))
                (y (pos 'y)))
            (fill-ellipse! x y width height color)))))

    (define (shape_rectangle rectangle color_name)
      (let ((color  (find-color color_name))
            (width  (rectangle 'width))
            (height (rectangle 'height))
            (pos    (rectangle 'position)))
        (lambda (delta)
          (let ((x (pos 'x))
                (y (pos 'y)))
            (fill-rectangle! x y width height color)))))

    (define (shape_image rectangle path)
      (let ((image (make-image path 'png/alpha))
            (pos   (rectangle 'position)))
        (lambda (delta)
          (let ((x (pos 'x))
                (y (pos 'y)))
            (draw-image! x y image)))))

    ;; Private

    dispatch)
