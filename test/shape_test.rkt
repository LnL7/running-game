#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../shape.rkt")



(test-case
  "ShapeEllipse"
  (let* ((display_engine (MakeStub 'ellipse))
         (physics_engine (MakeStub 'ellipse))
         (pos            (MakePosition 1 2))
         (vel            (MakeVelocity 3 4))
         (ellipse        (MakeEllipse pos vel 5 6)))
    (check-eq? (ellipse 'type)     'ellipse)
    (check-eq? (ellipse 'position) pos)
    (check-eq? (ellipse 'velocity) vel)
    (check-eq? (ellipse 'width)    5)
    (check-eq? (ellipse 'height)   6)
    (check-not-exn (lambda () (ellipse 'render display_engine)))
    (check-not-exn (lambda () (ellipse 'update! physics_engine)))
    (check-exn
      exn:fail?
      (lambda () (ellipse 'foobar)))))


(test-case
  "ShapeRectangle"
  (let* ((display_engine (MakeStub 'rectangle))
         (physics_engine (MakeStub 'rectangle))
         (pos            (MakePosition 1 2))
         (vel            (MakeVelocity 3 4))
         (rectangle      (MakeRectangle pos vel 5 6)))
    (check-eq? (rectangle 'type)     'rectangle)
    (check-eq? (rectangle 'position) pos)
    (check-eq? (rectangle 'velocity) vel)
    (check-eq? (rectangle 'width)    5)
    (check-eq? (rectangle 'height)   6)
    (check-not-exn (lambda () (rectangle 'render display_engine)))
    (check-not-exn (lambda () (rectangle 'update! physics_engine)))
    (check-exn
      exn:fail?
      (lambda () (rectangle 'foobar)))))


(test-case
  "ShapePoint"
  (let* ((pos   (MakePosition 1 2))
         (vel   (MakeVelocity 3 4))
         (point (MakePoint pos vel 5 6)))
    (check-eq? (point 'type)   'ellipse)
    (check-eq? (point 'width)  3)
    (check-eq? (point 'height) 3)))
