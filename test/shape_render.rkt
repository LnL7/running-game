#lang racket/base
(require "../shape.rkt")
(require "../console.rkt")
(require "../display.rkt")



(let* ((pos       (MakePosition 50 10))
       (vel       (MakeVelocity 0 0))
       (rectangle (MakeRectangle pos vel 100 50 'red)))
  (rectangle 'render (MakeConsole))
  (rectangle 'render (MakeDisplay 200 200)))

(let* ((pos     (MakePosition 50 10))
       (vel     (MakeVelocity 0 0))
       (ellipse (MakeEllipse pos vel 100 50 'blue)))
  (ellipse 'render (MakeConsole))
  (ellipse 'render (MakeDisplay 200 200)))
