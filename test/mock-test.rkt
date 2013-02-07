#lang racket/base
(require rackunit
         "../mock.rkt")


(test-case
  "Mock"
  (let ((mock (MakeMock 'one 'two)))
    (check-equal? (mock 'messages) '())
    (mock 'one)
    (mock 'two)
    (check-equal? (mock 'messages) '(one two))
    (check-equal? (mock 'messages) '())
    (check-exn
      exn:fail?
      (lambda () (mock 'foo)))))
