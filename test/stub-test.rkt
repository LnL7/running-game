#lang racket/base
(require rackunit)
(require "../stub.rkt")



(test-case
  "Stub"
  (let* ((proc (lambda args -3))
         (stub (MakeStub 'one '(two -2) (list 'three proc))))
    (check-true (stub 'one))
    (check-true (stub 'one 1))
    (check-eq?  (stub 'two)     -2)
    (check-eq?  (stub 'two 2)   -2)
    (check-eq?  (stub 'three)   -3)
    (check-eq?  (stub 'three 3) -3)
    (check-exn
      exn:fail?
      (lambda () (stub 'foo)))))
