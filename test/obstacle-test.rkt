#lang racket/base
(require rackunit)
(require "../mock.rkt")
(require "../obstacle.rkt")



(test-case
  "Obstacle"
  (let* ((pos         (MakePosition 1 2))
         (obstacle    (MakeObstacle pos 100 50))
         (engine-proc (lambda args (lambda args #t)))
         (engine-mock (MakeMock
                        (list 'rectangle engine-proc)
                        (list 'collide engine-proc))))
    (check-eq? (obstacle 'position)  pos)
    (obstacle 'render engine-mock)
    (check-equal? (engine-mock 'messages) '(rectangle))
    (obstacle 'update! 0 engine-mock)
    (check-equal? (engine-mock 'messages) '(collide rectangle))
    (check-exn exn:fail?  (lambda () (obstacle 'foobar)))))
