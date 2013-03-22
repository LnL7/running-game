#lang racket/base
(require "logger.rkt"
         "game.rkt")

(let* ((logger (MakeLogger #:fatal #f #:warn #f #:debug #f))
       (game   (MakeGame #:log logger)))
  (game 'start))
