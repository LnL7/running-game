#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../player.rkt")



(test-case
  "Player"
  (let* ((display-engine (MakeStub 'image))
         (physics-engine (MakeStub 'rectangle))
         (pos            (MakePosition 1 2))
         (player         (MakePlayer pos)))
    (check-not-exn (lambda () (player 'render display-engine)))
    (check-not-exn (lambda () (player 'update! physics-engine)))
    (check-exn
      exn:fail?
      (lambda () (player 'foobar)))))
