#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "velocity.rkt")
(provide MakeLevel MakeDefaultLevel)


(define (MakeDefaultLevel #:log [-log (MakeLogger)])
  (let ((level (MakeLevel #:log -log)))
    (level 'friction! -10)
    (level 'gravity!  (MakeVelocity 0 (* -0.94 4)))
    level))

(define (MakeLevel #:log [-log (MakeLogger)])
  (let ((-gravity  kNullVelocity)
        (-friction kNullNumber))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((friction)  get-friction)
          ((gravity)   get-gravity)
          ((friction!) set-friction!)
          ((gravity!)  set-gravity!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (get-friction) (if -friction -friction (-log 'warn "property not initialized" 'gravity kClass)))
    (define (get-gravity)  (if -gravity  -gravity  (-log 'warn "property not initialized" 'gravity kClass)))

    (define (set-friction! friction) (set! -friction friction))
    (define (set-gravity! gravity)   (set! -gravity gravity))


    ;; Private

    dispatch))


(define kClass        'Level)
(define kNullNumber   0)
(define kNullVelocity (MakeVelocity 0 0))
