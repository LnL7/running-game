#lang racket/base
(require "../shape.rkt")
(require "../console.rkt")
(require "../display.rkt")



(define c (MakeConsole))
(define d (MakeDisplay 300 100))

(let* ((pos       (MakePosition 10 10))
       (vel       (MakeVelocity 0 1))
       (rectangle (MakeRectangle pos vel 100 50 'red)))
  (rectangle 'render c)
  (rectangle 'render d))

(let* ((pos     (MakePosition 150 10))
       (vel     (MakeVelocity 0 1))
       (ellipse (MakeEllipse pos vel 100 50 'blue)))
  (ellipse 'render c)
  (ellipse 'render d))
