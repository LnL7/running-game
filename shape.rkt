#lang racket/base
(require "velocity.rkt"
         "position.rkt"
         "helpers.rkt")
(provide (all-from-out "velocity.rkt"
                       "position.rkt")
         MakeEllipse
         MakeRectangle
         MakeImage)



(define (MakeEllipse position width height . opts)
  (let ((color (default_color_helper opts)))
    (MakeShape 'ellipse position width height color)))

(define (MakeRectangle position width height . opts)
  (let ((color (default_color_helper opts)))
    (MakeShape 'rectangle position width height color)))

(define (MakeImage position width height path)
  (MakeShape 'image position width height path))


(define (MakeShape type position width height color_or_path)
  (let ((_type type)
        (_position position)
        (_width    width)
        (_height   height))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((type)     get_type)
          ((position) get_position)
          ((width)    get_width)
          ((height)   get_height)
          ((render)   render)
          ((move!)    move!)
          (else
            (method_missing msg 'Shape)))
        args))

    (define (get_type)     _type)
    (define (get_position) _position)
    (define (get_width)    _width)
    (define (get_height)   _height)

    (define (render engine)
      (engine type dispatch color_or_path))

    (define (move! engine velocity)
      (engine type dispatch velocity))

    dispatch))



(define (default_color_helper opts)
  (if (null? opts)
    COLOR
    (car opts)))

(define COLOR "white")
