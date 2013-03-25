#lang racket/base
(require "logger.rkt")
(provide Queue (all-from-out "logger.rkt"))


(define (MakeQueue -size -nil)
  (let ((-class Queue)
        (-mdtr  Mediator)
        (-head  0)
        (-tail  0)
        (-store (make-vector -size -nil)))
    (define (Queue msg . args)
      (case msg
        ((nil?)    (nil? dispatch))
        ((class)   -class)
        ((size)    -size)
        ((inspect) (inspect))
        ((empty?)  (apply empty? args))
        ((full?)   (apply full? args))
        ((each)    (apply each args))
        ((peek)    (apply peek args))
        ((pop!)    (apply pop! args))
        ((push!)   (apply push! args))
        (else      (-mdtr 'emit 'Queue.NoMethod msg dispatch))))
    (define dispatch Queue)

    (define (empty?) (eq? -tail -head))
    (define (full?)  (eq? (ref+ -tail) -head))

    (define (each proc!)
      (let --iter ((ctr -head))
        (cond
          ((eq? ctr -tail) Nil)
          (else
            (proc! (vector-ref -store ctr))
            (--iter (ref+ ctr))))))

    (define (peek)
      (cond
        ((empty?) (-mdtr 'emit 'Queue.EmptyCollection dispatch 'peek) -nil)
        (else     (vector-ref -store -head))))

    (define (pop!)
      (cond
        ((empty?) (-mdtr 'emit 'Queue.EmptyCollection dispatch 'pop!) -nil)
        ((let ((value (vector-ref -store -head)))
           (vector-set! -store -head -nil)
           (set! -head (ref+ -head))
           value))))

    (define (push! value)
      (cond
        ((full?) (-mdtr 'emit 'Queue.FullCollection dispatch 'push!))
        (else
          (vector-set! -store -tail value)
          (set! -tail (ref+ -tail))))
      Nil)

    (define (inspect)
      (vector 'Queue -head -tail -store))


    ;; Private
    (define (ref+ ref) (modulo (+ ref 1) -size))
    (-mdtr 'emit 'Queue.Initialized dispatch)
    dispatch))


(define Queue
  (let ((-mdtr Mediator))
    (define (Queue:Class msg . args)
      (case msg
        ((nil?) (nil? dispatch))
        ((new)  (apply alloc args))
        (else
          (-mdtr 'emit 'Queue.NoMethod msg dispatch))))
    (define dispatch Queue:Class)
    (define alloc MakeQueue)

    ;; Private
    dispatch))
