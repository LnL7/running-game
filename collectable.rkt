#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "velocity.rkt"
         "position.rkt"
         "shape.rkt")
(provide MakeRandomCollectable)


(define (MakeRandomCollectable offset speed x y #:log [-log (MakeLogger)])
  (let ((vel (MakeVelocity 0 (speed 'random) #:log -log))
        (pos (MakePosition (x 'random) (y 'random) #:log -log)))
    (offset 'y! (pos 'y))
    (MakeCollectable pos vel #:log -log)))


(define (MakeCollectable -position -velocity #:log [-log (MakeLogger)])
  (let ((-animation     0)
        (-display-shape #f)
        (-physics-shape #f)
        (-gravity-proc  #f)
        (-collide-proc  #f)
        (-animate-proc  #f))
    (define (Collectable msg . args)
      (case msg
        ((position) -position)
        ((render)   (apply render args))
        ((update!)  (apply update! args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Collectable)

    (define (render engine)
      (unless -display-shape (set! -display-shape (MakeImage -position kSize kSize (path-helper 1) #:log -log)))
      (-display-shape 'render engine))

    (define (update! score! delta engine)
      (unless -physics-shape (set! -physics-shape (MakeRectangle -position kSize kSize #:log -log)))
      (unless -gravity-proc  (set! -gravity-proc  (engine 'gravity 'collectable-mass -position -velocity)))
      (unless -collide-proc  (set! -collide-proc  (engine 'collide (collide score!) kSize kSize -position)))
      (unless -animate-proc  (set! -animate-proc  (animate)))
      (-gravity-proc delta)
      (-collide-proc delta)
      (-animate-proc delta)
      (-physics-shape 'update! delta engine -velocity))

    ;; Private
    (define (collide score!)
      (lambda (menu!) ;; soft and hard collisions
        (when -animation
          (set! -animation #f) ;; Stop animation
          (score!) (score!) (score!) (score!) (score!) ;; 5 times
          (-velocity 'vertical! kCollideBounce)
          (-log 'debug "score!" dispatch))
        #f)) ;; collision done

    (define (animate)
      (let ((ctr 0))
        (lambda (delta)
          (cond
            ((< ctr kAnimateSpeed) (set! ctr (+ ctr delta))) ;; Collect time delta
            (-animation ;; Only if animation is active
              (set! ctr 0)
              (animate-next!)
              (image-path! -animation))))))

    (define (animate-next!) ;; Set next animation
      (set! -animation (modulo (+ -animation 1) kPathMax)))

    (define (image-path! idx) ;; Set image path
      (-display-shape 'path! (path-helper idx)))

    ; (-log 'debug "initialized" dispatch)
    dispatch))


(define (path-helper idx) (string-append kPath (number->string idx) ".png"))

(define kSize          10)
(define kPath          "resources/coin")
(define kPathMax       3)
(define kAnimateSpeed  1)
(define kCollideBounce 10)
