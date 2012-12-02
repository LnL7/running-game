#lang racket/base
(require rackunit)
(require "../stub.rkt")



(test-case
  "Stub"
  (let ((stub (MakeStub 'one 'two)))
    (check-not-exn (lambda () (stub 'one)))
    (check-not-exn (lambda () (stub 'two)))
    (check-exn
      exn:fail?
      (lambda () (stub 'three)))))
