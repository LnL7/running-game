#lang racket/base
(require rackunit)
(require "../mock.rkt")
(require "../shape.rkt")



(test-case
  "ShapeRectangle"
  (let* ((pos         (MakePosition 1 2))
         (vel         (MakeVelocity 3 4))
         (rectangle   (MakeRectangle pos 5 6))
         (engine-proc (lambda args (lambda args #t)))
         (engine-mock (MakeMock (list 'rectangle engine-proc))))
    (check-eq? (rectangle 'type)          'rectangle)
    (check-eq? (rectangle 'position)      pos)
    (check-eq? (rectangle 'width)         5)
    (check-eq? (rectangle 'height)        6)
    (rectangle 'render engine-mock)
    (check-equal? (engine-mock 'messages) '(rectangle))
    (rectangle 'update! 0 engine-mock vel)
    (check-equal? (engine-mock 'messages) '(rectangle))
    (check-exn
      exn:fail?
      (lambda () (rectangle 'foobar)))))


(test-case
  "ShapeEllipse"
  (let* ((pos         (MakePosition 1 2))
         (vel         (MakeVelocity 3 4))
         (ellipse     (MakeEllipse pos 5 6))
         (engine-proc (lambda args (lambda args #t)))
         (engine-mock (MakeMock (list 'ellipse engine-proc))))
    (check-eq? (ellipse 'type)            'ellipse)
    (check-eq? (ellipse 'position)        pos)
    (check-eq? (ellipse 'width)           5)
    (check-eq? (ellipse 'height)          6)
    (ellipse 'render engine-mock)
    (check-equal? (engine-mock 'messages) '(ellipse))
    (ellipse 'update! 0 engine-mock vel)
    (check-equal? (engine-mock 'messages) '(ellipse))
    (check-exn
      exn:fail?
      (lambda () (ellipse 'foobar)))))


(test-case
  "ShapeImage"
  (let* ((pos         (MakePosition 1 2))
         (vel         (MakeVelocity 3 4))
         (image       (MakeImage pos 5 6 "example.png"))
         (engine-proc (lambda args (lambda args #t)))
         (engine-mock (MakeMock (list 'image engine-proc))))
    (check-eq? (image 'type) 'image)
    (check-eq? (image 'position) pos)
    (check-eq? (image 'width)    5)
    (check-eq? (image 'height)   6)
    (image 'render engine-mock)
    (check-equal? (engine-mock 'messages) '(image))
    (image 'update! 0 engine-mock vel)
    (check-equal? (engine-mock 'messages) '(image))
    (check-exn
      exn:fail?
      (lambda () (image 'foobar)))))
