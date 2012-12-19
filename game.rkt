#lang racket/base
(require "lib/canvas.rkt"
         "player.rkt"
         "display.rkt"
         "physics.rkt"
         "helpers.rkt")
(provide MakeGame)



(define (MakeGame)
  (let ((_player  #f)
        (_display (MakeDisplay))
        (_physics (MakePhysics)))
    (define (dispatch msg . args)
      (apply
        (case msg
          ((player) start_player)
          ((loop)   start_loop)
          (else
            (method_missing msg dispatch)))
        args))

    (define (start_player)
      (let ((position (MakePosition 200 200)))
        (set! _player (MakePlayer position))))

    (define (start_loop)
      (start-game-loop
        (lambda (delta)
          (let ((objects (list _player)))
            (for-each
              (lambda (obj) ((object_loop obj) delta))
              objects)))
        #t))

    ;; Private

    (define (object_loop obj)
      (if obj
        (lambda (delta)
          ((obj 'render _display))
          ((obj 'update! _physics) delta))
        (lambda (delta) #f)))


    dispatch))