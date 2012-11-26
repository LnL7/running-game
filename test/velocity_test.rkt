#lang racket/base
(require rackunit)
(require "../velocity.rkt")

(test-case
  "Velocity base"
  (let ((v (MakeVelocity 1 2)))
    (check-eq? (send v 'horizontal) 1)
    (check-eq? (send v 'vertical) 2)))


(test-case
  "Velocity.scale!"
  (let ((v (MakeVelocity 1 2)))
    (send v 'scale! -1)
    (check-eq? (send v 'horizontal) -1)
    (check-eq? (send v 'vertical) -2)))

(test-case
  "Velocity.add!"
  (let ((v (MakeVelocity 1 2)))
    (send v 'add! (MakeVelocity 2 1))
    (check-eq? (send v 'horizontal) 3)
    (check-eq? (send v 'vertical) 3)))
