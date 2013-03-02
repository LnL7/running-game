#lang racket/base
(require "helpers.rkt"
         "logger.rkt")
(provide MakePersistent)


(define (MakePersistent file #:log [-log (MakeLogger)])
  (let ((path (string-append "resources/" file ".txt")))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((read)   read-data)
          ((write!) write-data)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    ;; Properties

    (define (read-data)
      (let* ((in  (open-input-file path))
             (res (read in)))
        (close-input-port in)
        res))

    (define (write-data . values)
      (let ((out (open-output-file path #:exists 'replace)))
        (write values out)
        (close-output-port out)
        dispatch))


    ;; Private

    (-log 'debug "initialized" kClass)

    dispatch))


(define kClass     'Persistent)
(define kNullValue '())
