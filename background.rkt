#lang racket/base
(require "logger.rkt"
         "shape.rkt"
         "helpers.rkt")
(provide (all-from-out "shape.rkt")
         MakeBackground)


(define (MakeBackground #:log [-log (MakeLogger)])
  (let ((-display-shape #f)
        (-position kNullPosition))
    (define (Background msg . args)
      (case msg
        ((position) -position)
        ((render)   (apply render args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Background)

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeImage -position kWidth kHeight kPath #:log -log)))
      (-display-shape 'render engine))

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))


(define kWidth        800)
(define kHeight       600)
(define kPath         "resources/grass.jpg")
(define kNullPosition (MakePosition 0 0))
