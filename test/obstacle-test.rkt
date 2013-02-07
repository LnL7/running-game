#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../obstacle.rkt")



(test-case
  "Obstacle"
  (let* ((display-engine (MakeStub 'rectangle))
         (physics-engine (MakeStub 'rectangle))
         (pos            (MakePosition 1 2))
         (obstacle       (MakeObstacle pos 100 50)))
    (check-eq? (obstacle 'position)  pos)
    (check-not-exn (lambda ()        (obstacle 'render display-engine)))
    (check-not-exn (lambda ()        (obstacle 'update! physics-engine)))
    (check-exn exn:fail?  (lambda () (obstacle 'foobar)))))
