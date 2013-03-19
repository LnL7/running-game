#lang racket/base
(provide Nil nil?)


(define Nil
  (let ()
    (define (Nil msg . args)
      (case msg
        ((nil?)  (nil? dispatch))
        ((maybe) (apply maybe args))
        (else    dispatch))) ;; handles `class' and `new'
    (define dispatch Nil)
    (define (maybe object) (or object dispatch))

    ;; Private
    dispatch))


(define (nil? object) (eq? object Nil))
