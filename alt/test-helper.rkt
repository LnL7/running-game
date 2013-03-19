#lang racket/base
(require "logger.rkt"
         rackunit)
(provide mdtr
         test?
         initialized?
         no-method?
         full-collection?
         empty-collection?
         (all-from-out rackunit))


(define mdtr Mediator)
(let ((lg (Logger 'new mdtr)))
  (lg 'NoMethod 'Fatal)
  (lg 'FullCollection 'Fatal)
  (lg 'EmptyCollection 'Fatal)
  (void))


(define test?             (regexp "^Test *"))
(define initialized?      (regexp "^Initialized: *"))

(define no-method?        (regexp "^NoMethod *"))
(define full-collection?  (regexp "^FullCollection *"))
(define empty-collection? (regexp "^EmptyCollection *"))
