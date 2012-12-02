#lang racket/base
(require "../player.rkt")
(require "../console.rkt")
(require "../display.rkt")


(let* ((pos    (MakePosition 50 10))
       (player (MakePlayer pos)))
  (player 'render (MakeConsole))
  (player 'render (MakeDisplay 200 200)))
