#lang racket/base
(require rackunit)
(require "../position.rkt")
(require "../velocity.rkt")



(test-case
  "Position"
  (let ((position (MakePosition 1 2)))
    (check-eq? (send position 'x) 1)
    (check-eq? (send position 'y) 2)
    (send position 'x! 2)
    (send position 'y! 1)
    (check-eq? (send position 'x) 2)
    (check-eq? (send position 'y) 1)
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
