#lang racket/base
(require "../shape.rkt")
(require "../console.rkt")
(require "../display.rkt")


(let ((console (MakeConsole))
      (canvas  (MakeDisplay 200 200)))

  (let* ((pos       (MakePosition 50 10))
         (vel       (MakeVelocity 0 0))
         (rectangle (MakeRectangle pos vel 100 50 'red)))
    (rectangle 'render console)
    (rectangle 'render canvas))

  (let* ((pos     (MakePosition 50 70))
         (vel     (MakeVelocity 0 0))
         (ellipse (MakeEllipse pos vel 100 50 'blue)))
    (ellipse 'render console)
    (ellipse 'render canvas)))
