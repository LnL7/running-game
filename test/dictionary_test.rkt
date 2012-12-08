#lang racket/base
(require rackunit)
(require "../dictionary.rkt")


(test-case
  "Pair"
  (let* ((value (lambda () #t))
         (pair  (MakePair 1 value)))
    (check-eq? (pair 'key)   1)
    (check-eq? (pair 'value) value)))


(test-case
  "Dictionary"
  (let* ((value      (lambda () #t))
         (dictionary (MakeDictionary eq? 3)))
    (check-not-exn (lambda ()
                     (dictionary 'insert! (MakePair 1 (lambda () #f)))
                     (dictionary 'insert! (MakePair 2 (lambda () #f)))
                     (dictionary 'insert! (MakePair 1 (lambda () #t)))
                     (dictionary 'insert! (MakePair 3 value))))
    (check-eq?  (dictionary 'value 3) value)
    (check-true ((dictionary 'value 1)))
    (check-not-exn (lambda () (dictionary 'remove! 1)))
    (check-exn
      exn:fail?
      (lambda () (dictionary 'value 1)))
    (check-exn
      exn:fail?
      (lambda () (dictionary 'foobar)))
    ))
