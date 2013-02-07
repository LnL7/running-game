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



(define (MakeEllipse position width height [color kColor] #:log [-log (MakeLogger)] . opts)
  (MakeShape 'ellipse position width height color #:log -log))

(define (MakeRectangle position width height [color kColor] #:log [-log (MakeLogger)] . opts)
  (MakeShape 'rectangle position width height color #:log -log))

(define (MakeImage position width height path #:log [-log (MakeLogger)])
  (MakeShape 'image position width height path #:log -log))


(define (MakeShape type position width height [color-or-path kColor] #:log [-log (MakeLogger)])
  (let ((-render-proc   #f)
        (-update-proc   #f)
        (-type          type)
        (-position      position)
        (-width         width)
        (-height        height)
        (-color-or-path color-or-path))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((type)     get-type)
          ((position) get-position)
          ((width)    get-width)
          ((height)   get-height)
          ((color)    get-color-or-path)
          ((path)     get-color-or-path)
          ((color!)   set-color-or-path!)
          ((path!)    set-color-or-path!)
          ((render)   render)
          ((update!)  update!)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (get-type)     -type)
    (define (get-position) -position)
    (define (get-width)    -width)
    (define (get-height)   -height)

    (define (get-color-or-path)
      -color-or-path)

    (define (set-color-or-path! color-or-path)
      (set! -color-or-path color-or-path))

    (define (render engine)
      (unless -render-proc (set! -render-proc (engine type dispatch)))
      (-render-proc))

    (define (update! delta engine velocity)
      (unless -update-proc (set! -update-proc (engine type dispatch velocity)))
      (-update-proc delta))


    ;; Private

    ; (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Shape)
(define kColor "white")
