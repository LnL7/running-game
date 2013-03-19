#lang racket/base
(require "mediator.rkt"
         "test-helper.rkt")


(let ((_ Mediator)) ;; Class
  (test-case
    "Mediator."
    (check-false (_ 'nil?))
    (check-eq? ((_ 'new) 'class) _)
    ))


(let ((_ (Mediator 'new))) ;; Instance
  (test-case
    "Mediator#"
    (check-false (_ 'nil?))
    (check-true ((_ 'on 'Test
                    (lambda ()
                      (error 'Test "foo")))
                 'nil?))

    (check-exn test? (lambda () (_ 'emit 'Test)))

    (_ 'alias 'Foo.Test 'Test)
    (check-exn test? (lambda () (_ 'emit 'Foo.Test)))

    (_ 'prefix 'Bar 'Test)
    (check-exn test? (lambda () (_ 'emit 'Bar.Test)))
    ))
