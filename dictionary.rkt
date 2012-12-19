#lang racket/base
(require "helpers.rkt")
(provide MakeDictionary MakePair)



(define (MakeDictionary ==? _size)
  (let ((_next    0)
        (_vector  (make-vector _size)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((insert!) insert_pair!)
          ((remove!) remove_for_key)
          ((value)   value_for_key)
          (else
            (method_missing msg dispatch)))
        args))

    (define (insert_pair! pair)
      (let* ((key   (pair 'key))
             (index (index_for_key key)))
        (if index
          (vector-set! _vector index pair)
          (begin
            (vector-set! _vector _next pair)
            (set! _next (+ _next 1))))))

    (define (remove_for_key key)
      (let ((index (index_for_key key)))
        (set! _next (- _next 1))
        (let ((swap (vector-ref _vector _next)))
          (vector-set! _vector index swap))))

    (define (value_for_key key)
      (let ((pair (get_pair (index_for_key key))))
        (pair 'value)))

    ;; Private

    (define (index_for_key key)
      (let __iter ((index 0))
        (if (>= index _next)
          #f
          (let ((pair (get_pair index)))
            (if (==? (pair 'key) key)
              index
              (__iter (+ index 1)))))))

    (define (get_pair index)
      (vector-ref _vector index))

    dispatch))



(define (MakePair _key _value)
  (define (dispatch msg . args)
    (apply
      (case msg
        ((key)    get_key)
        ((value)  get_value)
        ((key!)   set_key!)
        ((value!) set_value!)
        (else
          (error msg "method missing ~a" dispatch)))
      args))

  (define (get_key)   _key)
  (define (get_value) _value)

  (define (set_key! key)
    (set! _key key))

  (define (set_value! value)
    (set! _value value))

  dispatch)