#lang racket/base
(require "nil.rkt"
         rackunit)


(let ((_ Nil))
  (test-case
    "Nil"
    (check-true (_ 'nil?))
    (check-eq? (_ 'foobar)   _)
    (check-eq? (_ 'maybe #f) _)
    (check-eq? (_ 'maybe #t) #t)
    ))
