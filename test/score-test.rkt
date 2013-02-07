#lang racket/base
(require rackunit)
(require "../mock.rkt")
(require "../score.rkt")



(test-case
  "Score"
  (let* ((score       (MakeScore))
         (engine-proc (lambda args (lambda args #t)))
         (engine-mock (MakeMock (list 'score engine-proc))))
    (check-eq? (score 'current) 0)
    (check-eq? (score 'highest) 0)
    (score 'render engine-mock)
    (check-equal? (engine-mock 'messages) '(score))
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
