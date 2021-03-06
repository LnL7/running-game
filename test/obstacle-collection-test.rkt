#lang racket/base
(require rackunit)
(require "../mock.rkt"
         "../obstacle-collection.rkt")



(test-case
  "Obstacle Collection"
  (let* ((collection   (MakeObstacleCollection))
         (engine-proc  (lambda args (lambda args #t)))
         (engine-mock  (MakeMock
                         (list 'collide engine-proc)
                         (list 'rectangle engine-proc))))
    ; (collection 'fill!) ;; Needs a Level
    (collection 'render engine-mock)
    ; (check memq 'rectangle (engine-mock 'messages)) ;; Empty
    (collection 'update! 0 engine-mock)
    ; (let ((messages (engine-mock 'messages))) ;; Empty
    ;   (check memq 'collide messages)
    ;   (check memq 'rectangle messages))
    (check-exn
      exn:fail?
      (lambda () (collection 'foobar)))))

