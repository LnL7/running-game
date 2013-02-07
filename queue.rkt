#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeQueue)



(define (MakeQueue size #:log [-log (MakeLogger)])
  (let* ((-size  (+ size 1))
         (-first 0)
         (-last  0)
         (-store (make-vector (+ size 1) 'null-queue-element)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((enqueue!) enqueue-element!)
          ((serve!)   serve-element!)
          ((peek)     peek-element)
          ((each)     each-element)
          ((empty?)   is-empty?)
          ((full?)    is-full?)
          (else
            (-log 'fatal "method missing" msg kClass)))
        args))

    (define (enqueue-element! elem)
      (if (is-full?)
        (-log 'warn "to a full queue" 'enqueue! elem)
        (begin
          (vector-set! -store -last elem)
          (set! -last (next-ref -last))
          ; (-log 'debug "increased last-ref to" kClass -last)
          #t)))

    (define (serve-element!)
      (if (is-empty?)
        (-log 'warn "to an empty queue" 'serve!)
        (let ((elem (vector-ref -store -first)))
          (set! -first (next-ref -first))
          ; (-log 'debug "increased first-ref to" kClass -first)
          elem)))

    (define (peek-element)
      (if (is-empty?)
        (-log 'warn "to an empty queue" 'peek)
        (vector-ref -store -first)))

    (define (each-element proc)
      (when (is-empty?) (-log 'warn "to an empty queue" 'each))
      (let --iter ((ctr -first))
        (unless (eq? ctr -last)
          (proc (vector-ref -store ctr))
          (--iter (next-ref ctr)))))

    (define (is-empty?)
      (eq? -last -first))

    (define (is-full?)
      (eq? (next-ref -last) -first))


    ;; Private

    (define (next-ref ref)
      (modulo (+ ref 1) -size))

    ; (-log 'debug "initialized" kClass)

    dispatch))


(define kClass 'Queue)
