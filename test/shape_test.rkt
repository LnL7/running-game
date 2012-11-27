#lang racket/base
(require rackunit)
(require "../shape.rkt")

(test-case
  "ShapeEllipse base"
  (let* ((pos     (MakePosition 1 2))
         (vel     (MakeVelocity 3 4))
         (width   5)
         (height  6)
         (ellipse (MakeEllipse pos vel width height)))
    (check-eq? (send ellipse 'type)     'ellipse)
    (check-eq? (send ellipse 'position) pos)
    (check-eq? (send ellipse 'velocity) vel)
    (check-eq? (send ellipse 'width)    width)
    (check-eq? (send ellipse 'height)   height)))

(test-case
  "ShapeRectangle base"
  (let* ((pos       (MakePosition 1 2))
         (vel       (MakeVelocity 3 4))
         (width     5)
         (height    6)
         (rectangle (MakeRectangle pos vel width height)))
    (check-eq? (send rectangle 'type)     'rectangle)
    (check-eq? (send rectangle 'position) pos)
    (check-eq? (send rectangle 'velocity) vel)
    (check-eq? (send rectangle 'width)    width)
    (check-eq? (send rectangle 'height)   height)))
