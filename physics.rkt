#lang racket/base
(require "logger.rkt"
         "level.rkt"
         "velocity.rkt"
         "update-engine.rkt"
         "collision-engine.rkt"
         "helpers.rkt")
(provide MakePhysics)



(define (MakePhysics #:log [-log (MakeLogger)])
  (let ((-game      #f)
        (-player    #f)
        (-level     #f)
        (-update    (MakeUpdateEngine #:log -log))
        (-collision (MakeCollisionEngine #:log -log)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((game!)     set-game!)
          ((level!)    set-level!)
          ((player!)   set-player!)
          ((rectangle) shape-rectangle)
          ((gravity)   gravity)
          ((reset)     reset)
          ((collide)   collide)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (set-game! game)     (set! -game game))
    (define (set-level! level)   (set! -level level))
    (define (set-player! player) (set! -player player))


    (define (shape-rectangle . args)
      (lambda (delta)
        (apply -update 'rectangle delta args)))

    (define (gravity . args)
      (let ((friction         (-level 'friction))
            (gravity-velocity (-level 'gravity)))
        (lambda (delta)
          (apply -update 'gravity delta friction gravity-velocity args))))

    (define (reset . args)
      (lambda (delta)
        (apply -collision 'reset delta args)))

    (define (collide . args)
      (lambda (delta)
        (apply -collision 'collide delta -game -player args)))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Physics)
