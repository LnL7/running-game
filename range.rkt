#lang racket/base
(require "helpers.rkt"
         "logger.rkt")
(provide MakeRange)



(define (MakeRange -from -to #:log [-log (MakeLogger)])
  (let ((-offset kNullNumber)
        (-length kNullNumber))
    (define (Range msg . args)
      (case msg
        ((to)       -to)
        ((from)     -from)
        ((length)   -length)
        ((offset)   -offset)
        ((to!)      (apply set-to! args))
        ((from!)    (apply set-from! args))
        ((offset!)  (apply set-offset! args))
        ((include?) (apply is-included? args))
        ((random)   (apply generate-random args))
        ((encode)   (apply encode args))
        ((decode)   (apply decode args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Range)

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

    (define (encode) ;; Serialize object to a Vector
      (vector 'range -from -to))

    (define (decode vect) ;; Deserialize object from a Vector
      (unless (eq? (vector-ref vect 0) 'range) (-log 'warn "wrong encoded vector for" dispatch))
      (set-from! (vector-ref vect 1))
      (set-to!   (vector-ref vect 2)))

    ;; Private
    (define (update) ;; updates calculated properties
      (set! -length (- -to -from))
      (when (< -length 0) (-log 'warn "not a valid range" 'update dispatch)))

    (update)
    (-log 'debug "initialized" dispatch)
    dispatch))


(define kNullNumber 0)
