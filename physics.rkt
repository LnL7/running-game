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
    (define (Physics msg . args)
      (case msg
        ((game!)     (apply set-game! args))
        ((level!)    (apply set-level! args))
        ((player!)   (apply set-player! args))
        ((rectangle) (apply shape-rectangle args))
        ((gravity)   (apply gravity args))
        ((reset)     (apply reset args))
        ((collide)   (apply collide args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Physics)

    (define (set-game! game)     (set! -game game))
    (define (set-level! level)   (set! -level level))
    (define (set-player! player) (set! -player player))

    (define (shape-rectangle . args)
      (lambda (delta)
        (apply -update 'rectangle delta args)))

    (define (gravity get-mass . args)
      (let ((mass     (-level get-mass))
            (friction (-level 'friction))
            (gravity  (-level 'gravity)))
        (lambda (delta)
          (apply -update 'gravity delta mass friction gravity args))))

    (define (reset . args)
      (lambda (delta)
        (apply -collision 'reset delta (-level 'player-bounce) args)))

    (define (collide . args)
      (lambda (delta)
        (apply -collision 'collide delta (-level 'player-bounce) -game -player args)))


    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))
