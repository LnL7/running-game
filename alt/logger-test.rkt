#lang racket/base
(require "logger.rkt"
         "test-helper.rkt")


(let ()
  (mdtr 'prefix 'Logger 'NoMethod)
  (void))


(let ((_ (Logger 'new mdtr))) ;; Instance
  (test-case
    "Logger#"
    (check-false (_ 'nil?))
    (check-exn no-method? (lambda () (_ 'foo)))
    ))


(let ((_ Logger)) ;; Class
  (test-case
    "Logger."
    (check-false (_ 'nil?))
    (check-eq? ((_ 'new mdtr) 'class) _)
    (check-exn no-method? (lambda () (_ 'foo)))
    ))
