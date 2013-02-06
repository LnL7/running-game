#lang racket/base
(provide chain)


(define (chain object . messages)
  (if (null? messages)
    object
    (let ((head (car messages))
          (tail (cdr messages)))
      (apply chain (object head) tail))))
