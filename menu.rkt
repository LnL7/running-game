#lang racket/base
(require "logger.rkt"
         "position.rkt"
         "helpers.rkt")
(provide MakeMenu)


(define (MakeMenu -score #:log [-log (MakeLogger)])
  (let ((-score-message-proc #f)
        (-high-message-proc  #f)
        (-score-proc         #f)
        (-high-proc          #f))
    (define (Menu msg . args)
      (case msg
        ((render) (apply render args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Menu)

    (define (render engine)
      (unless -score-message-proc (set! -score-message-proc (engine 'text kScoreMessage kScoreMessagePosition kColor)))
      (unless -high-message-proc  (set! -high-message-proc  (engine 'text kHighMessage kHighMessgePosition kColor)))
      (unless -score-proc         (set! -score-proc   (engine 'text (number->string (-score 'current)) kScorePosition kColor)))
      (unless -high-proc          (set! -high-proc    (engine 'text (number->string (-score 'highest)) kHighPosition kColor)))
      (-score-message-proc)
      (-high-message-proc)
      (-score-proc)
      (-high-proc)
      dispatch)

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))


(define (score-helper score)
  (number->string (score 'current)))


(define kScoreMessagePosition (MakePosition 100 400))
(define kHighMessgePosition   (MakePosition 100 200))
(define kScorePosition        (MakePosition 400 400))
(define kHighPosition         (MakePosition 400 200))
(define kScoreMessage         "Score:")
(define kHighMessage          "Highest:")
(define kColor                "black")
