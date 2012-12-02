#lang racket/base
(provide MakeStub)



(define (MakeStub . methods)
  (let ((_ctr 0))
    (define (dispatch msg . args)
      (apply
        (let __iter ((lst methods))
          (if (null? lst)
            (error msg "method missing ~a" dispatch)
            (let ((head (car lst)))
              (if (eq? msg head)
                stub
                (__iter (cdr lst))))))
        args))

    (define (stub . _)
      (set! _ctr (+ _ctr 1))
      _ctr)

    dispatch))
