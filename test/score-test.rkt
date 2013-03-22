#lang racket/base
(require rackunit)
(require "../mock.rkt")
(require "../score.rkt")



(test-case
  "Score"
  (let ((score (MakeScore)))
    (check-eq? (score 'current) 0)
    (check-eq? (score 'highest) 0)
    (check-exn
      exn:fail?
      (lambda () (score 'foobar)))))


(test-case
  "Score.add"
  (let ((score (MakeScore)))
    (score 'add)
    (check-eq? (score 'current) 1)
    (check-eq? (score 'highest) 0)))


(test-case
  "Score.end"
  (let ((score (MakeScore)))
    (score 'add)
    (score 'end)
    (check-eq? (score 'current) 0)
    (check-eq? (score 'highest) 1)))
