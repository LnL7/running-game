#lang racket/base
(require "helpers.rkt"
         "logger.rkt")
(provide MakeRange)



(define (MakeRange from to #:log [-log (MakeLogger)])
  (let ((-to     to)
        (-from   from)
        (-offset kNullNumber)
        (-length kNullNumber))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((to)       get-to)
          ((from)     get-from)
          ((to!)      set-to!)
          ((from!)    set-from!)
          ((offset)   get-offset)
          ((offset!)  set-offset!)
          ((length)   get-length)
          ((include?) is-included?)
          ((random)   generate-random)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (get-to)     -to)
    (define (get-from)   -from)
    (define (get-offset) -offset)
    (define (get-length) -length)

    (define (set-to! to)         (set! -to to)         (update))
    (define (set-from! from)     (set! -from from)     (update))
    (define (set-offset! offset) (set! -offset offset) (update))


    ;; (number-in-range -> boolean)
    (define (is-included? number)
      (and
        (>= (+ -offset -to) number)
        (<= (+ -offset -from) number)))

    ;; (-> number-is-included)
    (define (generate-random)
      (+ -offset (random (+ -length 1)) -from))


    ;; Private

    ;; updates calculated properties
    (define (update)
      (set! -length (- -to -from))
      (when (< -length 0) (-log 'fatal "not a valid range" 'update kClass)))

    (update)

    dispatch))


(define kClass      'Range)
(define kNullNumber 0)
