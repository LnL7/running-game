#lang racket/base
(require "logger.rkt"
         "score.rkt"
         "position.rkt"
         "helpers.rkt")
(provide MakeMenu)



(define (MakeMenu #:log [-log (MakeLogger)])
  (let ((-message-proc #f)
        (-score-proc   #f)
        (-score        (MakeScore)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((score)  apply-score)
          ((render) render)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (apply-score object)
      (object 'score! -score))

    (define (render engine)
      (unless -message-proc (set! -message-proc (engine 'text (MakePosition 250 400 #:log -log) "Game Over!" "black")))
      (unless -score-proc   (set! -score-proc   (engine 'text (MakePosition 400 300 #:log -log)
                                                        (number->string (-score 'current)) "black")))
      (-message-proc)
      (-score-proc))


    ;; Private

    dispatch))


(define kClass 'Menu)
