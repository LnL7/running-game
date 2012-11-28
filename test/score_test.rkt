#lang racket/base
(require rackunit)
(require "../score.rkt")



(test-case
  "Score"
  (let ((score (MakeScore)))
    (check-eq? (send score 'current) 0)
    (check-eq? (send score 'highest) 0)
    (check-exn
      exn:fail?
      (lambda () (send score 'foobar)))))


(test-case
  "Score.add"
  (let ((score (MakeScore)))
    (send score 'add)
    (check-eq? (send score 'current) 1)
    (check-eq? (send score 'highest) 0)))


(test-case
  "Score.end"
  (let ((score (MakeScore)))
    (send score 'add)
    (send score 'end)
    (check-eq? (send score 'current) 0)
    (check-eq? (send score 'highest) 1)))
