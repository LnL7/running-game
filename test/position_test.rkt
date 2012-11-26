#lang racket/base
(require rackunit)
(require "../position.rkt")
(require "../velocity.rkt")


(test-case
  "Position base"
  (let ((p (MakePosition 1 2)))
    (check-eq? (send p 'x) 1)
    (check-eq? (send p 'y) 2)
    (send p 'x! 2)
    (send p 'y! 1)
    (check-eq? (send p 'x) 2)
    (check-eq? (send p 'y) 1)))

(test-case
  "Position.distance"
  (let ((p1 (MakePosition 1 1))
        (p2 (MakePosition 1 3)))
    (check-eq? (send p1 'distance p2) 2)))

(test-case
  "Position.move!"
  (let ((p (MakePosition 1 2))
        (v (MakeVelocity 1 2)))
    (send p 'move! v)
    (check-eq? (send p 'x) 2)
    (check-eq? (send p 'y) 4)))
