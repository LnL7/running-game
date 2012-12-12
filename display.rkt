#lang racket/base
(require "lib/graphics.rkt")
(provide MakeDisplay)



(define (MakeDisplay width height)
  (let ((_canvas (make-graphics width height))
        (_width  width)
        (_height height)
        (_objects '())
        (_looping #f))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((rectangle) shape_rectangle)
          ((ellipse)   shape_ellipse)
          ((add!)      add_object!)
          ((remove!)   remove_object!)
          ((start!)    start_loop!)
          (else
            (error msg "method missing ~a" dispatch)))
        args))

    (define (shape_rectangle rectangle color)
      (lambda (delta)
        (let* ((width  (rectangle 'width))
               (height (rectangle 'height))
               (pos    (rectangle 'position))
               (x      (pos 'x))
               (y      (pos 'y)))
          (use-brush _canvas (to_brush color))
          (draw-rectangle _canvas x y width height))))

    (define (shape_ellipse ellipse color)
      (lambda (delta)
        (let* ((width  (ellipse 'width))
               (height (ellipse 'height))
               (pos    (ellipse 'position))
               (x      (pos 'x))
               (y      (pos 'y)))
          (use-brush _canvas (to_brush color))
          (draw-ellipse _canvas x y width height))))

    (define (add_object! obj)
      (let ((pair (cons obj (obj 'render dispatch))))
        (set! _objects (cons pair _objects))))

    (define (remove_object! obj)
      (display "TODO"))

    (define (start_loop!)
      (unless _looping
        (begin
          (set! _looping #t)
          (chain _canvas animations [add! object_loop])
          (chain _canvas animations [start]))))


    (define (object_loop delta)
      (let __iter ((lst _objects))
        (unless (null? lst)
          (let ((pair (car lst)))
            ((cdr pair) delta)
            (__iter (cdr lst))))))

    (define (to_brush sym)
      (let ((str (symbol->string sym)))
        (new brush% [color str])))

    dispatch))
