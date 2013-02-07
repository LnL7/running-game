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

  (define (shape-rectangle rectangle)
    (let ((width  (rectangle 'width))
          (height (rectangle 'height))
          (pos    (rectangle 'position)))
      (lambda ()
        (let ((x     (pos 'x))
              (y     (pos 'y))
              (color (find-color (rectangle 'color))))
          (fill-rectangle! x y width height color)))))

  (define (shape-ellipse ellipse)
    (let ((width  (ellipse 'width))
          (height (ellipse 'height))
          (pos    (ellipse 'position)))
      (lambda ()
        (let ((x     (pos 'x))
              (y     (pos 'y))
              (color (find-color (ellipse 'color))))
          (fill-ellipse! x y width height color)))))

  (define (shape-image rectangle)
    (let ((pos   (rectangle 'position)))
      (lambda ()
        (let ((x (pos 'x))
              (y (pos 'y))
              (image (make-image (rectangle 'path) 'png/alpha)))
          (draw-image! x y image)))))


  ;; Private

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass 'Display)
