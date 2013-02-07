#lang racket/base
(require rackunit)
(require "../stub.rkt"
         "../logger.rkt"
         "../queue.rkt")



(test-case
  "Queue"
  (let* ((logger (MakeLogger #:warn 'fatal))
         (queue  (MakeQueue 2 #:log logger)))
    (check-true (queue 'empty?))
    (queue 'enqueue! 1)
    (queue 'enqueue! 2)
    (check-true (queue 'full?))
    (check-exn exn:fail? (lambda () (queue 'enqueue! 3)))
    (check-eq?     (queue 'peek)    1)
    (check-eq?     (queue 'serve!)  1)
    (queue 'enqueue! 4)
    (check-eq?     (queue 'serve!)  2)
    (check-eq?     (queue 'serve!)  4)
    (check-exn exn:fail? (lambda () (queue 'serve!)))
    (check-exn exn:fail? (lambda () (queue 'foobar)))))
