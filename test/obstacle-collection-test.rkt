#lang racket/base
(require rackunit)
(require "../mock.rkt"
         "../obstacle-collection.rkt")


(define kRectangleList
  (let --iter ((ctr 0)
               (acc '()))
    (if (eq? ctr kObstacleCollectionSize)
      acc
      (--iter
        (+ ctr 1)
        (cons 'rectangle acc)))))


(test-case
  "Obstacle Collection"
  (let* ((collection   (MakeObstacleCollection))
         (engine-proc  (lambda args (lambda args #t)))
         (engine-mock  (MakeMock
                         (list 'rectangle engine-proc))))
    (collection 'fill!)
    (collection 'render engine-mock)
    (check-equal? (engine-mock 'messages) kRectangleList)
    (collection 'update! 0 engine-mock)
    (check-equal? (engine-mock 'messages) kRectangleList)
    (check-exn
      exn:fail?
      (lambda () (collection 'foobar)))))

