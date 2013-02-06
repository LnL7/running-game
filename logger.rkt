#lang racket/base
(require "helpers.rkt")
(provide MakeLogger)



(define (MakeLogger #:debug [-debug #f] #:warn [-warn #f] #:fatal [-fatal #t])
  (let ((-scope 0))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((position)  position)
          ((velocity)  velocity)
          ((ellipse)   shape-ellipse)
          ((rectangle) shape-rectangle)
          ((score)     score)
          ((debug)     debug)
          ((warn)      warning)
          ((fatal)     fatal)
          (else
            (warning "method missing" msg kClass)))
        args))

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

    (define (shape-ellipse ellipse color)
      (scope)
      (display "ShapeEllipse: <")
      (display color)
      (display ">")
      (shape ellipse))

    (define (shape-rectangle rectangle color)
      (scope)
      (display "ShapeRectangle: <")
      (display color)
      (display ">")
      (shape rectangle))

    (define (shape shape)
      (let ((pos    (shape 'position))
            (vel    (shape 'velocity))
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
        (velocity vel)
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

    (define (debug str . args)
      (when -debug
        (scope)
        (display "-----> ")
        (display str)
        (puts args)))

    (define (warning str . args)
      (when -warn
        (scope)
        (display "=====> ")
        (display str)
        (puts args)))

    (define (fatal message sym . args)
      (if -fatal
        (error sym (string-append message " ~a") args)
        (warning message)))


    ;; Private

    (define (puts args)
      (unless (null? args)
        (display " ")
        (display (car args))
        (puts (cdr args))))

    (define (scope)
      (newline)
      (let iter ((ctr -scope))
        (unless (= ctr 0)
          (display " . ")
          (iter (- ctr 1)))))

    (define (indent!) (set! -scope (+ -scope 1)))
    (define (dedent!) (set! -scope (- -scope 1)))

    dispatch))


(define kClass 'Logger)
