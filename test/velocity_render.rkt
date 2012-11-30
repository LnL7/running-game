#lang racket/base
(require "../velocity.rkt")
(require "../console.rkt")



(let ((velocity (MakeVelocity 5 10)))
  (velocity 'render (MakeConsole)))
