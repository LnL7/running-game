#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakePosition)


(define (MakePosition -x -y #:log [-log (MakeLogger)])
  (let ()
    (define (Position msg . args)
      (case msg
        ((x)        -x)
        ((y)        -y)
        ((x!)       (apply set-x! args))
        ((y!)       (apply set-y! args))
        ((distance) (apply calculate-distance args))
        ((copy)     (apply copy-position args))
        ((move!)    (apply move-velocity! args))
        ((render)   (apply render args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Position)

    (define (get-x) -x)
    (define (get-y) -y)

    (define (set-x! x) (set! -x x) dispatch)
    (define (set-y! y) (set! -y y) dispatch)

    (define (calculate-distance pos)
      (let ((x (- (pos 'x) -x))
            (y (- (pos 'y) -y)))
        (sqrt (+ (expt x 2) (expt y 2)))))

    (define (copy-position) (MakePosition -x -y))

    (define (move-velocity! vel)
      (set-x! (+ (vel 'horizontal) -x))
      (set-y! (+ (vel 'vertical)   -y))
      dispatch)

    (define (render engine)
      (engine 'position dispatch))

    ;; Private
    ; (-log 'debug "initialized" dispatch)
    dispatch))
