#lang racket/base
(require "helpers.rkt")
(provide MakeScore)



(define (MakeScore)
  (let ((_current 0)
        (_highest 0))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((current) get_current)
          ((highest) get_highest)
          ((add)     add_current)
          ((end)     end_current)
          ((render)  render)
          (else
            (method_missing msg dispatch)))
        args))

    (define (get_current) _current)
    (define (get_highest) _highest)

    (define (add_current)
      (set! _current (+ _current 1)))

    (define (end_current)
      (when (> _current _highest)
        (set! _highest _current))
      (set! _current 0))

    (define (render engine)
      (engine 'score dispatch))

    dispatch))
