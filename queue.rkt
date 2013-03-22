#lang racket/base
(require "logger.rkt"
         "helpers.rkt")
(provide MakeQueue)


(define (MakeQueue size #:log [-log (MakeLogger)])
  (let* ((-first 0)
         (-last  0)
         (-size  (+ size 1))
         (-store (make-vector (+ size 1) kNullElement)))
    (define (Queue msg . args)
      (case msg
        ((empty?)   (apply is-empty? args))
        ((full?)    (apply is-full? args))
        ((peek)     (apply peek-element args))
        ((each)     (apply each-element args))
        ((enqueue!) (apply enqueue-element! args))
        ((serve!)   (apply serve-element! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Queue)

    (define (is-empty?) (eq? -last -first))
    (define (is-full?)  (eq? (next-ref -last) -first))

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

    (define (enqueue-element! elem)
      (if (is-full?)
        (-log 'warn "to a full queue" 'enqueue! elem)
        (begin
          (vector-set! -store -last elem)
          (set! -last (next-ref -last))
          ; (-log 'debug "increased last-ref to" dispatch -last)
          #t)))

    (define (serve-element!)
      (if (is-empty?)
        (-log 'warn "to an empty queue" 'serve!)
        (let ((elem (vector-ref -store -first))) ;; get and return first
          (vector-set! -store -first kNullElement) ;; Remove actual reference
          (set! -first (next-ref -first))
          ; (-log 'debug "increased first-ref to" dispatch -first)
          elem)))

    ;; Private
    (define (next-ref ref) (modulo (+ ref 1) -size))
    ; (-log 'debug "initialized" dispatch)
    dispatch))


(define kNullElement 'queue-null-element)
