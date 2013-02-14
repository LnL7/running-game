#lang racket/base
(require rackunit
         "../stub.rkt"
         "../helpers.rkt")


(test-case
  "Helpers, chain"
  (let* ((two   (MakeStub '(one 1)))
         (three (MakeStub (list 'two (lambda args two)))))
    (check-eq? (chain three 'two 'one)           1)
    (check-eq? (chain three 'two ('one #f))      1)
    (check-eq? (chain three ('two #f) 'one)      1)
    (check-eq? (chain three ('two #f) ('one #f)) 1)))
