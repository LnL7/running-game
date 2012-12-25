#lang racket/base
(require "helpers.rkt")
(provide MakePosition)



(define (MakePosition x y)
  (let ((_x x)
        (_y y))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((x)        get_x)
          ((y)        get_y)
          ((x!)       set_x!)
          ((y!)       set_y!)
          ((distance) calculate_distance)
          ((copy)     copy_position)
          ((move!)    move_velocity!)
          ((render)   render)
          (else
            (method_missing msg dispatch)))
        args))

    (define (get_x) _x)
    (define (get_y) _y)

    (define (set_x! x) (set! _x x) dispatch)
    (define (set_y! y) (set! _y y) dispatch)

    (define (calculate_distance pos)
      (let ((x (- (pos 'x) _x))
            (y (- (pos 'y) _y)))
        (sqrt (+ (expt x 2) (expt y 2)))))

    (define (copy_position) (MakePosition _x _y))

    (define (move_velocity! vel)
      (set_x! (+ (vel 'horizontal) _x))
      (set_y! (+ (vel 'vertical)   _y))
      dispatch)

    (define (render engine)
      (engine 'position dispatch))

    ;; Private

    dispatch))
