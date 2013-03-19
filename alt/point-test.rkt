#lang racket/base
(require "point.rkt"
         "test-helper.rkt")


(let ()
  (mdtr 'prefix 'Point 'NoMethod)
  (void))


(let ((_ Point)) ;; Class
  (test-case
    "Point."
    (check-false (_ 'nil?))
    (check-eq? ((_ 'new 0 0) 'class) _)
    (check-eq? ((_ 'null) 'class)    _)
    (check-exn no-method? (lambda () (_ 'foo)))
    ))


(let ((_ (Point 'new 1 2))) ;; Instance
  (test-case
    "Point#"
    (check-false (_ 'nil?))
    (check-true ((_ 'x! 2) 'nil?))
    (check-true ((_ 'y! 4) 'nil?))
    (check-eq? (_ 'x) 2)
    (check-eq? (_ 'y) 4)
    (check-exn no-method? (lambda () (_ 'foo)))
    ))
