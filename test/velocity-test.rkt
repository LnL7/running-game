#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../velocity.rkt")



(test-case
  "Velocity"
  (let ((display-engine (MakeStub 'velocity))
        (velocity       (MakeVelocity 1 2)))
    (check-eq? (velocity 'horizontal) 1)
    (check-eq? (velocity 'vertical) 2)
    (check-exn
      exn:fail?
      (lambda () (velocity 'foobar)))))


(test-case
  "Velocity.scale!"
  (let ((velocity (MakeVelocity 1 2)))
    (velocity 'scale! -1)
    (check-eq? (velocity 'horizontal) -1)
    (check-eq? (velocity 'vertical) -2)))


(test-case
  "Velocity.add!"
  (let ((velocity (MakeVelocity 1 2)))
    (velocity 'add! (MakeVelocity 2 1))
    (check-eq? (velocity 'horizontal) 3)
    (check-eq? (velocity 'vertical) 3)))


(test-case
  "Velocity.copy"
  (let* ((velocity (MakeVelocity 1 2))
         (copy-vel (velocity 'copy)))
    (copy-vel 'add! (MakeVelocity 3 4))
    (check-eq? (velocity 'horizontal) 1)
    (check-eq? (velocity 'vertical)   2)))
