#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../position.rkt")
(require "../velocity.rkt")



(test-case
  "Position"
  (let ((display-engine (MakeStub 'position))
        (position       (MakePosition 1 2)))
    (check-eq? (position 'x) 1)
    (check-eq? (position 'y) 2)
    (check-not-exn (lambda () (position 'render display-engine)))
    (check-exn
      exn:fail?
      (lambda () (position 'foobar)))))


(test-case
  "Position.distance"
  (let ((pos-1 (MakePosition 1 1))
        (pos-2 (MakePosition 1 3)))
    (check-eq? (pos-1 'distance pos-2) 2)))


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
         (pos-copy (position 'copy)))
    (pos-copy 'move! (MakeVelocity 3 4))
    (check-eq? (position 'x) 1)
    (check-eq? (position 'y) 2)))
