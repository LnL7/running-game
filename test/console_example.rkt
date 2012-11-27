#lang racket/base
(require "../shape.rkt")
(require "../console.rkt")



(let* ((pos (MakePosition 10 10))
       (vel (MakeVelocity 0 1))
       (w 5)
       (h 6)
       (ellipse (MakeEllipse pos vel w h 'red)))
  (send ellipse 'render (MakeConsole)))
