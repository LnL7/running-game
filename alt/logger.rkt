#lang racket/base
(require "mediator.rkt")
(provide Logger (all-from-out "mediator.rkt"))


(define (MakeLogger -mdtr)
  (let ((-class Logger))
    (define (Logger msg . args)
      (case msg
        ((class)           -class)
        ((nil?)            (nil? dispatch))
        ((Fatal)           (-mdtr 'on msg (apply -class 'fatal args)))
        ((Warn)            (-mdtr 'on msg (apply -class 'warning args)))
        ((Debug)           (-mdtr 'on msg (apply -class 'debugging args)))
        ((Initialized)     (-mdtr 'on msg (apply initialized msg args)))
        ((NoMethod)        (-mdtr 'on msg (apply no-method msg args)))
        ((FullCollection)  (-mdtr 'on msg (apply collection-size msg args)))
        ((EmptyCollection) (-mdtr 'on msg (apply collection-size msg args)))
        (else              (-mdtr 'emit 'Logger.NoMethod msg dispatch))))
    (define dispatch Logger)

    (define (initialized source destination)
      (lambda (obj)
        (-mdtr 'emit destination source
               "~a" obj)))

    (define (no-method source destination)
      (lambda (msg obj)
        (-mdtr 'emit destination source
               "undefined method `~a' for ~a" msg obj)))

    (define (collection-size source destination)
      (lambda (obj meth)
        (-mdtr 'emit destination source
               "~a received `~a'" obj meth)))

    ;; Private
    (-mdtr 'emit 'Logger.Initialized dispatch)
    (dispatch 'Fatal)
    (dispatch 'Warn)
    (dispatch 'Debug)
    dispatch))


(define Logger
  (let ((-mdtr Mediator))
    (define (Logger:Class msg . args)
      (case msg
        ((fatal)     fatal)
        ((warning)   warning)
        ((debugging) debugging)
        ((nil?)      (nil? dispatch))
        ((new)       (apply alloc args))
        (else        (-mdtr 'emit 'Logger.NoMethod msg dispatch))))
    (define dispatch Logger:Class)
    (define alloc MakeLogger)

    (define (fatal sym str . args)
      (apply error sym str args)
      Nil)

    (define (warning sym str . args)
      (apply printf (string-append "-----> ~a: " str "\n") sym args)
      Nil)

    (define (debugging sym str . args)
      (apply printf (string-append "       ~a: " str "\n") sym args)
      Nil)

    ;; Private
    dispatch))
