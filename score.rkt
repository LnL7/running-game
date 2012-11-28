#lang racket/base
(require "helpers.rkt")
(provide (all-from-out "helpers.rkt")
         MakeScore)



(define (MakeScore)
  (let ((_current 0)
        (_highest 0))
    (define (dispatch msg)
      (cond
        ((eq? msg 'current) get_current)
        ((eq? msg 'highest) get_highest)
        ((eq? msg 'add)     add_current)
        ((eq? msg 'end)     end_current)
        ((eq? msg 'render)  render)
        (else
          (error msg "method missing ~a" dispatch))))

    (define (get_current) _current)
    (define (get_highest) _highest)

    (define (add_current)
      (set! _current (+ _current 1)))

    (define (end_current)
      (when (> _current _highest)
        (set! _highest _current))
      (set! _current 0))

    (define (render engine)
      (send engine 'score dispatch))

    dispatch))
