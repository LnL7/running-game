#lang racket/base
(require "../game.rkt")


(let ((game (MakeGame)))
  (game 'player)
  (game 'obstacles)
  (game 'loop))
