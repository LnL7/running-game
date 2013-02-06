#lang racket/base
(require "../logger.rkt"
         "../game.rkt")


(let* ((logger (MakeLogger #:debug #t #:warn #t))
       (game   (MakeGame #:log logger)))
  (game 'player)
  (game 'obstacles)
  (game 'loop))
