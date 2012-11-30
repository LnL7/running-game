#lang racket/base
(require rackunit)
(require "../score.rkt")
(require "../console.rkt")



(let ((score (MakeScore)))
  (score 'add)
  (score 'add)
  (score 'add)
  (score 'end)
  (score 'add)
  (score 'render (MakeConsole)))
