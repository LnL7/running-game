#lang racket/base
(require "helpers.rkt"
         "logger.rkt")
(provide MakeFile)


(define (MakeFile -name #:log [-log (MakeLogger)])
  (let ((-path (string-append kPathPrefix -name kPathSuffix)))
    (define (File msg . args)
      (case msg
        ((read)   (apply read-data args))
        ((write!) (apply write-data args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch File)

    (define (read-data)
      (-log 'warn "reading from disk" dispatch -name)
      (let* ((in  (open-input-file -path))
             (res (read in)))
        (close-input-port in)
        res))

    (define (write-data value)
      (-log 'warn "writing to disk" dispatch -name)
      (let ((out (open-output-file -path #:exists 'replace)))
        (write value out)
        (close-output-port out)
        dispatch))

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))


(define kNullValue  '())
(define kPathPrefix "resources/")
(define kPathSuffix ".txt")
