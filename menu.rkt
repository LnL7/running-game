#lang racket/base
(require "logger.rkt"
         "score.rkt"
         "position.rkt"
         "helpers.rkt")
(provide MakeMenu)



(define (MakeMenu #:log [-log (MakeLogger)])
  (let ((-message-proc #f)
        (-score-proc   #f)
        (-score        (MakeScore #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((score)  get-score)
          ((render) render)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties
    (define (get-score) -score)

    (define (render engine)
      (unless -message-proc (set! -message-proc (engine 'text kMessagePosition "Game Over!" kColor)))
      (unless -score-proc   (set! -score-proc   (engine 'text kScorePosition (-score 'current) kColor)))
      (-message-proc)
      (-score-proc)
      dispatch)


    ;; Private

    dispatch))


(define kClass           'Menu)
(define kMessagePosition (MakePosition 250 400))
(define kScorePosition   (MakePosition 400 300))
(define kMessage         "Game Over!")
(define kColor           "black")
