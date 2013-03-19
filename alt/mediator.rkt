#lang racket/base
(require "nil.rkt")
(provide Mediator (all-from-out "nil.rkt"))


(define (MakeMediator)
  (let ((-class    Mediator)
        (-channels (Mediator 'hash)))
    (define (Mediator msg . args)
      (case msg
        ((class)  -class)
        ((nil?)   (nil? dispatch))
        ((on)     (apply on args))
        ((emit)   (apply emit args))
        ((alias)  (apply alias args))
        ((prefix) (apply prefix args))
        (else     (emit 'Mediator.NoMethodError msg dispatch))))
    (define dispatch Mediator)

    (define (on channel proc!)
      (value! channel
              (cons proc! (value channel)))
      Nil)

    (define (emit channel . args)
      (apply mediator-emit channel args)
      (for-each (lambda (proc) (apply proc args))
                (value channel))
      Nil)

    (define (alias source destination)
      (on source (lambda args
                   (apply emit destination args)))
      Nil)

    (define (prefix prefix . destinations)
      (let --iter ((lst destinations))
        (cond
          ((null? lst) Nil)
          ((let* ((head   (car lst))
                  (source (-class 'namespace prefix head)))
             (alias source head)
             (--iter (cdr lst)))))))

    ;; Private
    (define (value . args)  (apply -class 'value -channels args))
    (define (value! . args) (apply -class 'value! -channels args))

    (define (mediator-emit channel . args)
      (case channel
        ((Mediator.Emit) #f)
        ((Fatal)         #f)
        ((Warn)          #f)
        ((Debug)         #f)
        (else            (emit 'Mediator.Emit 'Emit "`~a' with ~a" channel args))))

    dispatch))


(define Mediator
  (let ((-instance  #f)
        (-separator "."))
    (define (Mediator:Class msg . args)
      (case msg
        ((nil?)      (nil? dispatch))
        ((new)       (apply alloc args))
        ((instance)  (apply instance args))
        ((namespace) (apply namespace args))
        ((hash)      (apply hash args))
        ((value)     (apply value args))
        ((value!)    (apply value! args))
        (else        (apply (instance) msg args))))
    (define dispatch Mediator:Class)
    (define alloc MakeMediator)

    (define (instance)
      (cond ((not -instance) (set! -instance (alloc))))
      -instance)

    (define (namespace scope channel)
      (string->symbol
        (string-append
          (symbol->string scope)
          -separator
          (symbol->string channel))))

    (define (hash)               (make-hasheq))
    (define (value h key)        (hash-ref h key '()))
    (define (value! h key value) (hash-set! h key value))

    ;; R5RS compatible implementation ;;
    ;; ------------------------------ ;;
    ; (require srfi/69)
    ; (define (hash)               (make-hash-table))
    ; (define (value h key)        (hash-table-ref h key nil))
    ; (define (value! h key value) (hash-table-set! h key value))

    ;; Private
    ; (define (nil) '())
    dispatch))
