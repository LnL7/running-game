#lang racket/base
(require rackunit)
(require "../position.rkt")
(require "../velocity.rkt")



(test-case
  "Position"
  (let ((position (MakePosition 1 2)))
    (check-eq? (send position 'x) 1)
    (check-eq? (send position 'y) 2)
    (check-exn
      exn:fail?
      (lambda () (send position 'foobar)))))


(test-case
  "Position.distance"
  (let ((pos_1 (MakePosition 1 1))
        (pos_2 (MakePosition 1 3)))
    (check-eq? (send pos_1 'distance pos_2) 2)))


(test-case
  "Position.move!"
  (let ((vel (MakeVelocity 1 2))
        (position (MakePosition 1 2)))
    (send position 'move! vel)
    (check-eq? (send position 'x) 2)
    (check-eq? (send position 'y) 4)))


(test-case
  "Position.copy"
  (let* ((x 3)
         (y 4)
         (position (MakePosition x y))
         (pos_copy (send position 'copy)))
    (send pos_copy 'move! (MakeVelocity 1 2))
    (check-eq? (send position 'x) x)
    (check-eq? (send position 'y) y)))
