#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../shape.rkt")



(test-case
  "ShapeEllipse"
  (let* ((engine  (MakeStub 'ellipse))
         (pos     (MakePosition 1 2))
         (vel     (MakeVelocity 3 4))
         (ellipse (MakeEllipse pos 5 6)))
    (check-eq? (ellipse 'type)     'ellipse)
    (check-eq? (ellipse 'position) pos)
    (check-eq? (ellipse 'width)    5)
    (check-eq? (ellipse 'height)   6)
    (check-not-exn (lambda () (ellipse 'render engine)))
    (check-not-exn (lambda () (ellipse 'move! engine vel)))
    (check-exn
      exn:fail?
      (lambda () (ellipse 'foobar)))))


(test-case
  "ShapeRectangle"
  (let* ((engine (MakeStub 'rectangle))
         (pos            (MakePosition 1 2))
         (vel            (MakeVelocity 3 4))
         (rectangle      (MakeRectangle pos 5 6)))
    (check-eq? (rectangle 'type)     'rectangle)
    (check-eq? (rectangle 'position) pos)
    (check-eq? (rectangle 'width)    5)
    (check-eq? (rectangle 'height)   6)
    (check-not-exn (lambda () (rectangle 'render engine)))
    (check-not-exn (lambda () (rectangle 'move! engine vel)))
    (check-exn
      exn:fail?
      (lambda () (rectangle 'foobar)))))


(test-case
  "ShapeImage"
  (let* ((engine (MakeStub 'image))
         (pos            (MakePosition 1 2))
         (vel            (MakeVelocity 3 4))
         (image          (MakeImage pos 5 6 "example.png")))
    (check-eq? (image 'type) 'image)
    (check-eq? (image 'position) pos)
    (check-eq? (image 'width)    5)
    (check-eq? (image 'height)   6)
    (check-not-exn (lambda () (image 'render engine)))
    (check-not-exn (lambda () (image 'move! engine vel)))
    (check-exn
      exn:fail?
      (lambda () (image 'foobar)))))
