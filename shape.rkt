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
        (-update?       #f)
        (-type          type)
        (-position      position)
        (-width         width)
        (-height        height)
        (-color-or-path color-or-path))
    (define (Shape msg . args)
      (case msg
        ((type)     -type)
        ((position) -position)
        ((width)    -width)
        ((height)   -height)
        ((color)    -color-or-path)
        ((path)     -color-or-path)
        ((color!)   (apply set-color-or-path! args))
        ((path!)    (apply set-color-or-path! args))
        ((render)   (apply render args))
        ((update!)  (apply update! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Shape)

    (define (set-color-or-path! color-or-path)
      (set! -update? #t)
      (set! -color-or-path color-or-path))

    (define (render engine)
      (unless -render-proc (set! -render-proc (engine -type dispatch)))
      (-render-proc -update?)
      (set! -update? #f))

    (define (update! delta engine velocity)
      (unless -update-proc (set! -update-proc (engine -type dispatch velocity)))
      (-update-proc delta)
      (set! -update? #f))

    ;; Private
    ; (-log 'debug "initialized" dispatch)
    dispatch))


(define kColor "white")
