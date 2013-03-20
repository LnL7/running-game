#lang racket/base
(require "helpers.rkt"
         "logger.rkt")
(provide MakeFile)


(define (MakeFile name #:log [-log (MakeLogger)])
  (let ((-path (string-append "resources/" -name ".txt")))
    (define (File msg . args)
      (case msg
        ((read)   (apply read-data args))
        ((write!) (apply write-data args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch File)

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
    (-log 'debug "initialized" dispatch)
    dispatch))


(define kNullValue '())
