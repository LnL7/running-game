#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../obstacle.rkt")



(test-case
  "Obstacle"
  (let* ((display_engine (MakeStub 'rectangle))
         (physics_engine (MakeStub 'rectangle))
         (pos            (MakePosition 1 2))
         (obstacle       (MakeObstacle pos 100 50)))
    (check-not-exn (lambda () (obstacle 'render display_engine)))
    (check-not-exn (lambda () (obstacle 'update! physics_engine)))
    (check-exn
      exn:fail?
      (lambda () (obstacle 'foobar)))))
