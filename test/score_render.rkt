#lang racket/base
(require rackunit)
(require "../score.rkt")
(require "../console.rkt")



(let ((score (MakeScore)))
  (send score 'add)
  (send score 'add)
  (send score 'add)
  (send score 'end)
  (send score 'add)
  (send score 'render (MakeConsole)))
