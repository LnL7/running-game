#lang racket/base
(require "logger.rkt"
         "lib/canvas.rkt" ;; Library created by Sam Vervaeck
         "helpers.rkt")
(provide MakeScreenEngine
         (rename-out [kMessages kScreenEngineMessages]))


(define (MakeScreenEngine #:log [-log (MakeLogger)])
  (define (ScreenEngine msg . args)
    (case msg
      ((rectangle) (apply shape-rectangle args))
      ((ellipse)   (apply shape-ellipse args))
      ((image)     (apply shape-image args))
      ((text)      (apply text args))
      (else
        (lambda args (-log 'warn "method missing" msg dispatch)))))
  (define dispatch ScreenEngine)

  (define (shape-rectangle rectangle)
    (let ((width  (rectangle 'width))
          (height (rectangle 'height))
          (pos    (rectangle 'position)))
      (lambda (update?)
        (let ((x     (pos 'x))
              (y     (pos 'y))
              (color (find-color (rectangle 'color))))
          (fill-rectangle! x y width height color)))))

  (define (shape-ellipse ellipse)
    (let ((width  (ellipse 'width))
          (height (ellipse 'height))
          (pos    (ellipse 'position))
          (color (find-color (ellipse 'color))))
      (lambda (update?)
        (let ((x     (pos 'x))
              (y     (pos 'y)))
          (when update? (set! color (find-color (ellipse 'color)))) ;; refresh color
          (fill-ellipse! x y width height color)))))

  (define (shape-image rectangle)
    (let ((pos   (rectangle 'position))
          (image (make-image (rectangle 'path))))
        (lambda (update?)
          (when update? (set! image (make-image (rectangle 'path)))) ;; refresh image
          (let ((x (pos 'x))
                (y (pos 'y)))
                (draw-image! x y image)))))

  (define (text message pos color-name)
    (let ((font (make-font #:face kFontFace #:size kFontSize)))
      (lambda ()
        (let ((x    (pos 'x))
              (y    (pos 'y)))
          (draw-text! x y message (find-color color-name) font)))))

  ;; Private
  (-log 'debug "initialized" dispatch)
  dispatch)


(define kMessages '(rectangle ellipse image score text))
(define kFontFace "Verdana")
(define kFontSize 32)
