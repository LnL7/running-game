#lang racket/base
(require "../position.rkt")
(require "../console.rkt")



(let ((position (MakePosition 5 10)))
  (send position 'render (MakeConsole)))
