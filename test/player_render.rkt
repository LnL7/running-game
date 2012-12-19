#lang racket/base
(require "../lib/canvas.rkt")
(require "../player.rkt")
(require "../console.rkt")
(require "../display.rkt")



(let* ((pos    (MakePosition 100 200))
       (player (MakePlayer pos)))
  (player 'render (MakeConsole))
  (start-game-loop
    (let ((display (MakeDisplay)))
      (player 'render display))))
