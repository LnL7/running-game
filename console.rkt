#lang racket/base
(require "helpers.rkt")
(provide (all-from-out "helpers.rkt")
         MakeConsole)


(define (MakeConsole)
  (define (dispatch msg)
    (cond
      ((eq? msg 'position) position)
      ((eq? msg 'velocity) velocity)
      ((eq? msg 'warning)  warning)
      (else
        (error "method missing ~a" dispatch))))

  (define (position pos)
    (let ((x (send pos 'x))
          (y (send pos 'y)))
      (display "<Position x:")
      (display x)
      (display ", y:")
      (display y)
      (display ">")
      (newline)))

  (define (velocity vel)
    (let ((horizontal (send vel 'horizontal))
          (vertical   (send vel 'vertical)))
      (display "<Velocity h:")
      (display horizontal)
      (display ", v:")
      (display vertical)
      (display ">")
      (newline)))

  (define (warning str)
    (display "WARNING: ")
    (display str)
    (newline))

  dispatch)
