#lang racket/base
(require rackunit
         "../stub.rkt"
         "../helpers.rkt")


(test-case
  "Helpers, chain"
  (let* ((two   (MakeStub '(one 1)))
         (three (MakeStub (list 'two (lambda () two)))))
    (check-eq? (chain three 'two 'one) 1)))
