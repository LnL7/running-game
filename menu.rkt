#lang racket/base
(require "logger.rkt"
         "position.rkt"
         "helpers.rkt")
(provide MakeMenu)


(define (MakeMenu -score #:log [-log (MakeLogger)])
  (let ((-message-proc #f)
        (-score-proc   #f))
    (define (Menu msg . args)
      (case msg
        ((render) (apply render args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Menu)

    (define (render engine)
      (unless -message-proc (set! -message-proc (engine 'text kMessage kMessagePosition kColor)))
      (unless -score-proc   (set! -score-proc   (engine 'text (score-helper -score) kScorePosition kColor)))
      (-message-proc)
      (-score-proc)
      dispatch)

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))


(define (score-helper score)
  (number->string (score 'current)))


(define kMessagePosition (MakePosition 250 400))
(define kScorePosition   (MakePosition 400 300))
(define kMessage         "Game Over!")
(define kColor           "black")
