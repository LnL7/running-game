#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeConsoleEngine)



(define (MakeConsoleEngine #:log [-log (MakeLogger)])
  (let ((-scope 0))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((rectangle) shape-rectangle)
          ((ellipse)   shape-ellipse)
          ((position)  position)
          ((velocity)  velocity)
          ((image)     shape-image)
          ((score)     score)
          (else
            (-log 'warn "method missing" msg kClass)))
        args))

    (define (shape-ellipse ellipse)
      (scope)
      (display "ShapeEllipse: <")
      (display (ellipse 'color))
      (display ">")
      (shape ellipse))

    (define (shape-rectangle rectangle)
      (scope)
      (display "ShapeRectangle: <")
      (display (rectangle 'color))
      (display ">")
      (shape rectangle))

    (define (shape-image image)
      (scope)
      (display "ShapeImage: <")
      (display (image 'path))
      (display ">")
      (shape image))

    (define (position pos)
      (let ((x (pos 'x))
            (y (pos 'y)))
        (scope)
        (display "Position:")
        (indent!)
        (scope)
        (display "-x = ")
        (display x)
        (scope)
        (display "-y = ")
        (display y)
        (dedent!)))

    (define (velocity vel)
      (let ((horizontal (vel 'horizontal))
            (vertical   (vel 'vertical)))
        (scope)
        (display "Velocity:")
        (indent!)
        (scope)
        (display "-h = ")
        (display horizontal)
        (scope)
        (display "-v = ")
        (display vertical)
        (dedent!)))

    (define (score score)
      (let ((curr (score 'current))
            (high (score 'highest)))
        (scope)
        (display "Score:")
        (indent!)
        (scope)
        (display "-c: ")
        (display curr)
        (scope)
        (display "-h: ")
        (display high)
        (dedent!)))


    ;; Private

    (define (shape shape)
      (let ((pos    (shape 'position))
            (width  (shape 'width))
            (height (shape 'height)))
        (indent!)
        (scope)
        (display "-w = ")
        (display width)
        (scope)
        (display "-h = ")
        (display height)
        (position pos)
        (dedent!)))

    (define (scope)
      (newline)
      (let iter ((ctr -scope))
        (unless (= ctr 0)
          (display " . ")
          (iter (- ctr 1)))))

    (define (indent!) (set! -scope (+ -scope 1)))
    (define (dedent!) (set! -scope (- -scope 1)))

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'ConsoleEngine)
