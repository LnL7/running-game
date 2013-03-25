#lang racket/base
(require "queue.rkt"
         "test-helper.rkt")

(let ()
  (mdtr 'prefix 'Queue 'NoMethod 'FullCollection 'EmptyCollection)
  (void))


(let ((_ Queue)) ;; Class
  (test-case
    "Queue."
    (check-false (_ 'nil?))
    (check-eq? ((_ 'new 3 #f) 'class) _)
    (check-exn no-method? (lambda () (_ 'foo)))
    ))


(let ((_ (Queue 'new 3 #f))) ;; Instance
  (test-case
    "Queue#"
    (check-false (_ 'nil?))
    (check-true (_ 'empty?))
    (check-true ((_ 'push! 1) 'nil?))
    (check-true ((_ 'push! 2) 'nil?))
    (check-exn full-collection? (lambda () (_ 'push! 3)))
    (check-true (_ 'full?))
    (check-eq? (_ 'peek) 1)
    (check-eq? (_ 'pop!) 1)
    (check-eq? (_ 'pop!) 2)
    (check-exn empty-collection? (lambda () (_ 'peek)))
    (check-exn empty-collection? (lambda () (_ 'pop!)))
    (check-exn no-method? (lambda () (_ 'foo)))
    ))
