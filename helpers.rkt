#lang racket/base
(provide send)


(define (send obj msg . args)
  (apply (obj msg) args))
