#lang racket/base
(require "helpers.rkt")
(provide MakeLogger)



(define (MakeLogger #:debug [-debug #f] #:warn [-warn #t] #:fatal [-fatal #t])
  (define (dispatch msg . args)
    (apply
      (case msg
        ((debug) debug)
        ((warn)  warning)
        ((fatal) fatal)
        (else
          (lambda args (warning "method missing" msg kClass))))
      args))

  (define (debug message sym . args)
    (cond
      ((eq? -debug #f)  -debug)
      ((symbol? -debug) (apply dispatch -debug message sym args))
      (else
        (newline)
        (display "-----> ")
        (display message)
        (display " ")
        (display sym)
        (puts args))))

  (define (warning message sym . args)
    (cond
      ((eq? -warn #f)  -warn)
      ((symbol? -warn) (apply dispatch -warn message sym args))
      (else
        (newline)
        (display "=====> ")
        (display message)
        (display " ")
        (display sym)
        (puts args))))

  (define (fatal message sym . args)
    (cond
      ((eq? -fatal #f)  -fatal)
      ((symbol? -fatal) (apply dispatch -fatal message sym args))
      (else
        (error sym (string-append message " ~a") args))))


  ;; Private

  (define (puts args)
    (unless (null? args)
      (display ", ")
      (display (car args))
      (puts (cdr args))))

  dispatch)


(define kClass 'Logger)
