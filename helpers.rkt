#lang racket/base
(provide chain)



(define-syntax chain
  (syntax-rules ()
    [(chain expr)                                    expr]
    [(chain object ('method-msg args ...) exprs ...) (chain (object 'method-msg args ...) exprs ...)]
    [(chain object 'method-msg exprs ...)            (chain (object 'method-msg) exprs ...)]))
