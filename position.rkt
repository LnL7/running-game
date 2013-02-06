#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakePosition)



(define (MakePosition x y #:log [-log (MakeLogger)])
  (let ((-x x)
        (-y y))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((x)        get-x)
          ((y)        get-y)
          ((x!)       set-x!)
          ((y!)       set-y!)
          ((distance) calculate-distance)
          ((copy)     copy-position)
          ((move!)    move-velocity!)
          ((render)   render)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

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

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Position)
