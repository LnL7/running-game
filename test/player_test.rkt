#lang racket/base
(require rackunit)
(require "../stub.rkt")
(require "../player.rkt")



(test-case
  "Player"
  (let* ((display_engine (MakeStub 'ellipse))
         (physics_engine (MakeStub 'rectangle))
         (pos            (MakePosition 1 2))
         (player         (MakePlayer pos)))
    (check-not-exn (lambda () (player 'render display_engine)))
    (check-not-exn (lambda () (player 'update! physics_engine)))
    (check-exn
      exn:fail?
      (lambda () (player 'foobar)))))
