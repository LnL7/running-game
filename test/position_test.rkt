#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../position.rkt")
(require "../velocity.rkt")



(test-case
  "Position"
  (let ((display_engine (MakeStub 'position))
        (position       (MakePosition 1 2)))
    (check-eq? (position 'x) 1)
    (check-eq? (position 'y) 2)
    (check-not-exn (lambda () (position 'render display_engine)))
    (check-exn
      exn:fail?
      (lambda () (position 'foobar)))))


(test-case
  "Position.distance"
  (let ((pos_1 (MakePosition 1 1))
        (pos_2 (MakePosition 1 3)))
    (check-eq? (pos_1 'distance pos_2) 2)))


(test-case
  "Position.move!"
  (let ((vel (MakeVelocity 1 2))
        (position (MakePosition 1 2)))
    (position 'move! vel)
    (check-eq? (position 'x) 2)
    (check-eq? (position 'y) 4)))


(test-case
  "Position.copy"
  (let* ((position (MakePosition 1 2))
         (pos_copy (position 'copy)))
    (pos_copy 'move! (MakeVelocity 3 4))
    (check-eq? (position 'x) 1)
    (check-eq? (position 'y) 2)))
