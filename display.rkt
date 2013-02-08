#lang racket/base
(require "logger.rkt"
         "screen-engine.rkt"
         "console-engine.rkt"
         "helpers.rkt")
(provide MakeDisplay)



(define (MakeDisplay #:log [-log (MakeLogger)])
  (let ((-screen  (MakeScreenEngine #:log -log))
        (-console (MakeConsoleEngine #:log -log)))
    (define (dispatch msg . args)
      (apply
        (if (memq msg kScreenEngineMessages) -screen -console)
        msg
        args))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Display)
