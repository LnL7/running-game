#lang racket/base
(require "../shape.rkt")
(require "../console.rkt")



(define display (MakeConsole))

(let* ((pos    (MakePosition 10 10))
       (vel    (MakeVelocity 0 1))
       (circle (MakeEllipse pos vel 6 3 'red)))
  (send circle 'render display))
