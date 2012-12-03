#lang racket/base
(require "lib/graphics.rkt")
(provide MakeDisplay)



(define (MakeDisplay width height)
  (let ((_canvas (make-graphics width height))
        (_width  width)
        (_height height))
    (define (dispatch msg . args)
      (apply
        (cond
          ((eq? msg 'rectangle) shape_rectangle)
          ((eq? msg 'ellipse)   shape_ellipse)
          (else
            (error msg "method missing ~a" dispatch)))
        args))

    (define (shape_rectangle rectangle color)
      (start!
        (lambda (delta)
          (let* ((width  (rectangle 'width))
                 (height (rectangle 'height))
                 (pos    (rectangle 'position))
                 (x      (pos 'x))
                 (y      (pos 'y)))
            (use-brush _canvas (to_brush color))
            (draw-rectangle _canvas x y width height)))))

    (define (shape_ellipse ellipse color)
      (start!
        (lambda (delta)
          (let* ((width  (ellipse 'width))
                 (height (ellipse 'height))
                 (pos    (ellipse 'position))
                 (x      (pos 'x))
                 (y      (pos 'y)))
            (use-brush _canvas (to_brush color))
            (draw-ellipse _canvas x y width height)))))


    (define (start! event)
      (chain _canvas animations [add! event])
      (chain _canvas animations [start]))

    (define (to_brush sym)
      (let ((str (symbol->string sym)))
        (new brush% [color str])))

    dispatch))
