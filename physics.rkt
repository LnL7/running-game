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
          ((player!)   set-player!)
          ((rectangle) shape-rectangle)
          ((gravity)   gravity)
          ((reset)     reset)
          ((collide)   collide)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (shape-rectangle . args)
      (lambda (delta)
        (apply -update 'rectangle delta args)))

    (define (gravity . args)
      (lambda (delta)
        (apply -update 'gravity delta args)))

    (define (reset . args)
      (lambda (delta)
        (apply -collision 'reset delta args)))

    (define (collide . args)
      (lambda (delta)
        (apply -collision 'collide delta args)))

    (define (set-player! . args)
      (apply -collision 'player! args))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Physics)
