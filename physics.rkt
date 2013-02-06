#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "update-engine.rkt"
         "collision-engine.rkt"
         "helpers.rkt")
(provide MakePhysics)



(define (MakePhysics #:log [-log (MakeLogger)])
  (let ((-update    (MakeUpdateEngine #:log -log))
        (-collision (MakeCollisionEngine #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((rectangle) shape-rectangle)
          ((gravity)   gravity)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (shape-rectangle . args)
      (lambda (delta)
        (apply -update 'rectangle delta args)))

    (define (gravity . args)
      (apply -collision 'reset args)
      (lambda (delta)
        (apply -update 'gravity delta args)))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Physics)
