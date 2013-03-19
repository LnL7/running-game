#lang racket/base
(require "logger.rkt")
(provide Point (all-from-out "logger.rkt"))


(define (MakePoint -x -y)
  (let ((-class Point)
        (-mdtr  Mediator))
    (define (Point msg . args)
      (case msg
        ((nil?)    (nil? dispatch))
        ((class)   -class)
        ((x)       -x)
        ((y)       -y)
        ((x!)      (apply x! args))
        ((y!)      (apply y! args))
        (else    (-mdtr 'emit 'Point.NoMethod msg dispatch))))
    (define dispatch Point)

    (define (x! x) (set! -x x) Nil)
    (define (y! y) (set! -y y) Nil)

    ;; Private
    (-mdtr 'emit 'Point.Initialized dispatch)
    dispatch))


(define Point
  (let ((-mdtr Mediator)
        (-null 0))
    (define (Point:Class msg . args)
      (case msg
        ((nil?) (nil? dispatch))
        ((new)  (apply alloc args))
        ((null) (alloc-null))
        (else
          (-mdtr 'emit 'Point.NoMethod msg dispatch))))
    (define dispatch Point:Class)

    (define alloc MakePoint)
    (define (alloc-null) (alloc -null -null))

    ;; Private
    dispatch))
