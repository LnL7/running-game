#lang racket/base
(require rackunit)
(require "../stub.rkt"
         "../obstacle-collection.rkt")



(test-case
  "Obstacle Collection"
  (let* ((display-engine (MakeStub 'rectangle))
         (physics-engine (MakeStub 'rectangle))
         (collection     (MakeObstacleCollection)))
    (check-not-exn (lambda ()        (collection 'fill!)))
    (check-not-exn (lambda ()        (collection 'render display-engine)))
    (check-not-exn (lambda ()        (collection 'update! physics-engine)))
    (check-exn exn:fail?  (lambda () (collection 'foobar)))))
