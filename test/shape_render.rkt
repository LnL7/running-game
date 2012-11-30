#lang racket/base
(require "../shape.rkt")
(require "../console.rkt")



(let* ((pos (MakePosition 10 10))
       (vel (MakeVelocity 0 1))
       (rectangle (MakeRectangle pos vel 100 50 'red)))
  (rectangle 'render (MakeConsole)))
