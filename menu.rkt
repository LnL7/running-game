#lang racket/base
(require "logger.rkt"
         "position.rkt"
         "helpers.rkt")
(provide MakeMenu)


(define (MakeMenu -level #:log [-log (MakeLogger)])
  (let ((-level-message-proc #f)
        (-level-name-proc    #f)
        (-score-message-proc #f)
        (-high-message-proc  #f)
        (-score-proc         #f)
        (-high-proc          #f)
        (-new-proc           #f))
    (define (Menu msg . args)
      (case msg
        ((render) (apply render args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Menu)

    (define (render engine)
      (unless -level-message-proc (set! -level-message-proc (engine 'text kLevelMessage kLevelMesagePosition kColor)))
      (unless -level-name-proc    (set! -level-name-proc    (engine 'text (-level 'name) kLevelNamePosition kColor)))
      (unless -score-message-proc (set! -score-message-proc (engine 'text kScoreMessage kScoreMessagePosition kColor)))
      (unless -high-message-proc  (set! -high-message-proc  (engine 'text kHighMessage kHighMessgePosition kColor)))
      (unless -score-proc         (set! -score-proc         (engine 'text (number->string ((-level 'score) 'current)) kScorePosition kColor)))
      (unless -high-proc          (set! -high-proc          (engine 'text (number->string ((-level 'score) 'highest)) kHighPosition kColor)))
      (unless -new-proc           (set! -new-proc           (when (eq? ((-level 'score) 'current) ((-level 'score) 'highest))
                                                              (engine 'text kNewMessage kNewMessagePosition kColor))))
      (-level-message-proc)
      (-level-name-proc)
      (-score-message-proc)
      (-score-proc)
      (-high-message-proc)
      (-high-proc)
      (-new-proc)
      dispatch)

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))


(define (score-helper score)
  (number->string (score 'current)))


(define kLevelMesagePosition  (MakePosition 100 500))
(define kLevelNamePosition    (MakePosition 400 500))
(define kScoreMessagePosition (MakePosition 100 400))
(define kScorePosition        (MakePosition 400 400))
(define kHighMessgePosition   (MakePosition 100 300))
(define kHighPosition         (MakePosition 400 300))
(define kNewMessagePosition   (MakePosition 250 100))
(define kLevelMessage         "Level: ")
(define kScoreMessage         "Score:")
(define kHighMessage          "Highest:")
(define kNewMessage           "New Highscore!!!")
(define kColor                "black")
