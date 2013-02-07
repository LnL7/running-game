#lang racket/base
(require rackunit
         "../logger.rkt")


(test-case
  "Logger"
  (let ((full-log  (MakeLogger #:debug #t #:warn #t))
        (fatal-log (MakeLogger #:debug 'fatal #:warn 'fatal)))
    (check-not-exn (lambda ()       (full-log 'warn "testing..." 'full)))
    (check-not-exn (lambda ()       (full-log 'debug "testing..." 'full)))
    (check-exn exn:fail? (lambda () (fatal-log 'warn "testing..." 'fatal)))
    (check-exn exn:fail? (lambda () (fatal-log 'debug "testing..." 'fatal)))
    (check-exn exn:fail? (lambda () (fatal-log 'fatal "testing..." 'fatal)))))
