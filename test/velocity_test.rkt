#lang racket/base
(require rackunit)
(require "../velocity.rkt")



(test-case
  "Velocity"
  (let ((vel (MakeVelocity 1 2)))
    (check-eq? (send vel 'horizontal) 1)
    (check-eq? (send vel 'vertical) 2)
    (check-exn
      exn:fail?
      (lambda () (send vel 'foobar)))))


(test-case
  "Velocity.scale!"
  (let ((vel (MakeVelocity 1 2)))
    (send vel 'scale! -1)
    (check-eq? (send vel 'horizontal) -1)
    (check-eq? (send vel 'vertical) -2)))


(test-case
  "Velocity.add!"
  (let ((vel (MakeVelocity 1 2)))
    (send vel 'add! (MakeVelocity 2 1))
    (check-eq? (send vel 'horizontal) 3)
    (check-eq? (send vel 'vertical) 3)))


(test-case
  "Velocity.copy"
  (let* ((h 2)
         (v 3)
         (velocity (MakeVelocity h v))
         (copy_vel (send velocity 'copy)))
    (send copy_vel 'add! (MakeVelocity 1 2))
    (check-eq? (send velocity 'horizontal) h)
    (check-eq? (send velocity 'vertical)   v)))
