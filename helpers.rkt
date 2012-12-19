#lang racket/base
(provide method_missing)



(define (method_missing msg dispatch)
  (error msg "method missing ~a" dispatch))
