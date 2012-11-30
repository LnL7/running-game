#lang racket/base
(require rackunit)
(require "../velocity.rkt")



(test-case
  "Velocity"
  (let ((vel (MakeVelocity 1 2)))
    (check-eq? (vel 'horizontal) 1)
    (check-eq? (vel 'vertical) 2)
    (check-exn
      exn:fail?
      (lambda () (vel 'foobar)))))


(test-case
  "Velocity.scale!"
  (let ((vel (MakeVelocity 1 2)))
    (vel 'scale! -1)
    (check-eq? (vel 'horizontal) -1)
    (check-eq? (vel 'vertical) -2)))


(test-case
  "Velocity.add!"
  (let ((vel (MakeVelocity 1 2)))
    (vel 'add! (MakeVelocity 2 1))
    (check-eq? (vel 'horizontal) 3)
    (check-eq? (vel 'vertical) 3)))


(test-case
  "Velocity.copy"
  (let* ((velocity (MakeVelocity 1 2))
         (copy_vel (velocity 'copy)))
    (copy_vel 'add! (MakeVelocity 3 4))
    (check-eq? (velocity 'horizontal) 1)
    (check-eq? (velocity 'vertical)   2)))
