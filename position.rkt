#lang racket/base
(require "helpers.rkt")
(provide (all-from-out "helpers.rkt")
         MakePosition)



(define (MakePosition x y)
  (let ((_x x)
        (_y y))
    (define (dispatch msg)
      (cond
        ((eq? msg 'x)        get_x)
        ((eq? msg 'y)        get_y)
        ((eq? msg 'x!)       set_x!)
        ((eq? msg 'y!)       set_y!)
        ((eq? msg 'distance) calculate_distance)
        ((eq? msg 'move!)    move_velocity!)
        ((eq? msg 'render)    render)
        (else
          (error "method missing ~a" dispatch))))

    (define (get_x) _x)
    (define (get_y) _y)

    (define (set_x! x) (set! _x x) dispatch)
    (define (set_y! y) (set! _y y) dispatch)

    (define (calculate_distance pos)
      (let ((x (- (send pos 'x) _x))
            (y (- (send pos 'y) _y)))
        (sqrt (+ (expt x 2) (expt y 2)))))

    (define (move_velocity! vel)
      (set! _x (+ (send vel 'horizontal) _x))
      (set! _y (+ (send vel 'vertical) _y))
      dispatch)

    (define (render engine)
      (send engine 'position dispatch))

    dispatch))
