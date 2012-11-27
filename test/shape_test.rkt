#lang racket/base
(require rackunit)
(require "../shape.rkt")



(test-case
  "ShapeEllipse"
  (let* ((pos (MakePosition 1 2))
         (vel (MakeVelocity 3 4))
         (w 5)
         (h 6)
         (ellipse (MakeEllipse pos vel w h)))
    (check-eq? (send ellipse 'type)     'ellipse)
    (check-eq? (send ellipse 'position) pos)
    (check-eq? (send ellipse 'velocity) vel)
    (check-eq? (send ellipse 'width)    w)
    (check-eq? (send ellipse 'height)   h)
    (check-exn
      exn:fail?
      (lambda () (send ellipse 'foobar)))))


(test-case
  "ShapeRectangle"
  (let* ((pos (MakePosition 1 2))
         (vel (MakeVelocity 3 4))
         (w 5)
         (h 6)
         (rectangle (MakeRectangle pos vel w h)))
    (check-eq? (send rectangle 'type)     'rectangle)
    (check-eq? (send rectangle 'position) pos)
    (check-eq? (send rectangle 'velocity) vel)
    (check-eq? (send rectangle 'width)    w)
    (check-eq? (send rectangle 'height)   h)
    (check-exn
      exn:fail?
      (lambda () (send rectangle 'foobar)))))

