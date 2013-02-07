#lang racket/base
(require rackunit)
(require "../mock.rkt")
(require "../player.rkt")



(test-case
  "Player"
  (let* ((pos          (MakePosition 1 2))
         (player       (MakePlayer pos))
         (engine-proc  (lambda args (lambda args #t)))
         (engine-mock  (MakeMock
                         (list 'image engine-proc)
                         (list 'reset engine-proc)
                         (list 'gravity engine-proc)
                         (list 'rectangle engine-proc))))
    (player 'render engine-mock)
    (check-equal? (engine-mock 'messages) '(image))
    (player 'update! 0 engine-mock)
    (check-equal? (engine-mock 'messages) '(reset gravity rectangle))
    (check-exn
      exn:fail?
      (lambda () (player 'foobar)))))
