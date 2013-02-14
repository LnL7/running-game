#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt"
         "helpers.rkt")
(provide MakeScreenEngine
         (rename-out [kMessages kScreenEngineMessages]))



(define (MakeScreenEngine #:log [-log (MakeLogger)])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((rectangle) shape-rectangle)
        ((ellipse)   shape-ellipse)
        ((image)     shape-image)
        ((text)      text)
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
    (let ((pos (rectangle 'position)))
      (lambda ()
        (let ((x (pos 'x))
              (y (pos 'y))
              (image (make-image (rectangle 'path))))
          (draw-image! x y image)))))

  (define (text pos message color-name)
    (let ((font (make-font #:size 64 #:face "Verdana")))
      (lambda ()
        (let ((x    (pos 'x))
              (y    (pos 'y)))
          (draw-text! x y message (find-color color-name) font)))))


  ;; Private

  (-log 'debug "initialized" kClass)

  dispatch)


(define kClass    'ScreenEngine)
(define kMessages '(rectangle ellipse image text))
