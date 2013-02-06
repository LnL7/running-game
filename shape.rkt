#lang racket/base
(require "logger.rkt"
         "velocity.rkt"
         "position.rkt"
         "helpers.rkt")
(provide (all-from-out "velocity.rkt"
                       "position.rkt")
         MakeEllipse
         MakeRectangle
         MakeImage)



(define (MakeEllipse position width height #:log [-log (MakeLogger)] . opts)
  (let ((color (default-color-helper opts)))
    (MakeShape 'ellipse position width height color #:log -log)))

(define (MakeRectangle position width height #:log [-log (MakeLogger)] . opts)
  (let ((color (default-color-helper opts)))
    (MakeShape 'rectangle position width height color #:log -log)))

(define (MakeImage position width height path #:log [-log (MakeLogger)])
  (MakeShape 'image position width height path #:log -log))


(define (MakeShape type position width height color-or-path #:log [-log (MakeLogger)])
  (let ((-type     type)
        (-position position)
        (-width    width)
        (-height   height))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((type)     get-type)
          ((position) get-position)
          ((width)    get-width)
          ((height)   get-height)
          ((render)   render)
          ((move!)    move!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (get-type)     -type)
    (define (get-position) -position)
    (define (get-width)    -width)
    (define (get-height)   -height)

    (define (render engine)
      (engine type dispatch color-or-path))

    (define (move! engine velocity)
      (engine type dispatch velocity))

    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))



(define (default-color-helper opts)
  (if (null? opts)
    kColor
    (car opts)))


(define kClass 'Shape)
(define kColor "white")
