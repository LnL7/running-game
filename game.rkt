#lang racket/base
(require "lib/canvas.rkt")
(require "player.rkt")
(require "display.rkt")
(require "physics.rkt")
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
            (error msg "method missing ~a" dispatch)))
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
