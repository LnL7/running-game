#lang racket/base
(require "logger.rkt"
         "screen-engine.rkt"
         "console-engine.rkt"
         "helpers.rkt")
(provide MakeDisplay)



(define (MakeDisplay #:log [-log (MakeLogger)])
  (let ((-screen  (MakeScreenEngine #:log -log))
        (-console (MakeConsoleEngine #:log -log)))
    (define (Display msg . args)
      (apply
        (if (memq msg kScreenEngineMessages) -screen -console)
        msg
        args))
    (define dispatch Display)

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))
