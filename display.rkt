#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt"
         "helpers.rkt")
(provide MakeDisplay)



(define (MakeDisplay #:log [-log (MakeLogger)])
    (define (dispatch msg . args)
      (apply
        (case msg
          ((ellipse)   shape-ellipse)
          ((rectangle) shape-rectangle)
          ((image)     shape-image)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (shape-ellipse ellipse color-name)
      (let ((color  (find-color color-name))
            (width  (ellipse 'width))
            (height (ellipse 'height))
            (pos    (ellipse 'position)))
        (lambda (delta)
          (let ((x (pos 'x))
                (y (pos 'y)))
            (fill-ellipse! x y width height color)))))

    (define (shape-rectangle rectangle color-name)
      (let ((color  (find-color color-name))
            (width  (rectangle 'width))
            (height (rectangle 'height))
            (pos    (rectangle 'position)))
        (lambda (delta)
          (let ((x (pos 'x))
                (y (pos 'y)))
            (fill-rectangle! x y width height color)))))

    (define (shape-image rectangle path)
      (let ((image (make-image path 'png/alpha))
            (pos   (rectangle 'position)))
        (lambda (delta)
          (let ((x (pos 'x))
                (y (pos 'y)))
            (draw-image! x y image)))))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch)


(define kClass 'Display)
