#lang racket/base
(require "velocity.rkt"
         "position.rkt")
(provide (all-from-out "velocity.rkt"
                       "position.rkt")
         MakePoint
         MakeEllipse
         MakeRectangle)



(define (MakeEllipse position velocity width height . opts)
  (let ((color (default_color opts)))
    (MakeShape 'ellipse position velocity width height color)))

(define (MakeRectangle position velocity width height . opts)
  (let ((color (default_color opts)))
    (MakeShape 'rectangle position velocity width height color)))

(define (MakePoint position velocity . opts)
  (let ((color (default_color opts)))
    (MakeShape 'ellipse position velocity 3 3 color)))


(define (MakeShape type position velocity width height color)
  (let ((_type type)
        (_position position)
        (_velocity velocity)
        (_width    width)
        (_height   height))
    (define (dispatch msg . args)
      (apply
        (cond
          ((eq? msg 'type)     get_type)
          ((eq? msg 'position) get_position)
          ((eq? msg 'velocity) get_velocity)
          ((eq? msg 'width)    get_width)
          ((eq? msg 'height)   get_height)
          ((eq? msg 'render)   render)
          ((eq? msg 'update!)  update!)
          (else
            (error msg "method missing ~a" dispatch)))
        args))

    (define (get_type)     _type)
    (define (get_position) _position)
    (define (get_velocity) _velocity)
    (define (get_width)    _width)
    (define (get_height)   _height)

    (define (render engine)
      (engine type dispatch color))

    (define (update! engine)
      (engine type dispatch))

    dispatch))



(define (default_color opts)
  (if (null? opts)
    'white
    (car opts)))
