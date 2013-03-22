#lang racket/base
(require "lib/canvas.rkt" ;; Library Created by Sam Vervaeck
         "logger.rkt"
         "file.rkt"
         "background.rkt"
         "menu.rkt"
         "world.rkt"
         "level.rkt"
         "physics.rkt"
         "input.rkt"
         "display.rkt"
         "helpers.rkt")
(provide MakeGame)


(define (MakeGame #:log [-log (MakeLogger)])
  (let ((-state      #f)
        (-menu       #f)
        (-world      #f)
        (-levels     #f)
        (-level      kDefaultLevelIndex)
        (-input      (MakeInput #:log -log))
        (-display    (MakeDisplay #:log -log))
        (-physics    (MakePhysics #:log -log))
        (-background (MakeBackground #:log -log))
        (-files   (vector ;; serialized levels
                    (MakeFile kDefaultLevel #:log -log)
                    (MakeFile kSpeedyLevel #:log -log)
                    (MakeFile kSlowLevel #:log -log))))
    (define (Game msg . args)
      (case msg
        ((files)  -files)
        ((levels) -levels)
        ((end)    (apply end args))
        ((start)  (apply start args))
        ((menu)   (apply menu args))
        ((world)  (apply world args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Game)

    (define (end)   (stop-game-loop))
    (define (start) (start-game-loop game-loop #t))

    (define (menu) ;; change state to menu (scoreboard)
      (unless (eq? -state 'menu)
        (let ((level (vector-ref -levels -level))
              (file  (vector-ref -files -level)))
          (next-level!)
          (set! -state 'menu)
          (set! -world #f) ;; world not neede when menu is shown
          (set! -menu  (MakeMenu level file #:log -log))))
      dispatch)

    (define (world) ;; change state to world (play level)
      (unless (eq? -state 'world)
        (set! -state 'world)
        (set! -menu #f) ;; menu not neede when playing
        (set! -world (MakeWorld #:log -log))
        (let* ((level        (vector-ref -levels -level))
               (score        (level 'score))
               (player       (-world 'player))
               (obstacles    (-world 'obstacles))
               (collectables (-world 'collectables)))
          (-physics 'level! level)
          (-physics 'player! player)
          (-input 'level! level)
          (-input 'strafe player)
          (-input 'jump player)
          (-input 'slide player)
          (-input 'character player)
          (player 'level! level)
          (obstacles 'level! level)
          (obstacles 'fill!)
          (collectables 'level! level)
          (collectables 'fill!)))
      dispatch)

    ;; Private
    (define (game-loop delta) ;; main game loop
      (let ((percentage (scale-helper delta)))
        (case -state ;; delegate to correct loop
          ((menu)  (menu-loop delta))
          ((world) (world-loop delta))
          (else
            (-log 'fatal "invalid state" -state dispatch)))))

    (define grass (make-image "resources/grass.jpg"))

    (define (world-loop delta)
      (-background 'render -display)
      (-world 'render -display)
      (-world 'update! (scale-helper delta) -physics))

    (define (menu-loop delta)
      (-background 'render -display)
      (-menu 'render -display)
      (-menu 'update!))

    (define (next-level!)
      (set! -level (modulo
                     (+ -level 1)
                     (vector-length -levels))))

    (define (set-levels! proc!) ;; set level for each file
      (let --iter ((ctr (- (vector-length -files) 1)))
        (cond
          ((< ctr 0) #t)
          (else
            (let ((file  (vector-ref -files ctr)))
              (vector-set! -levels ctr (proc! file))
              (--iter (- ctr 1)))))))

    (set! -levels (make-vector (vector-length -files) #f)) ;; create correct amount of levels
    (set-levels! (lambda (file)
                   (let ((level (MakeLevel #:log -log)))
                     (level 'decode (file 'read)) ;; read level data
                     level))) ;; return level, used with vector-set!
    (-physics 'game! dispatch)
    (-input 'game! dispatch)
    (-input 'world)
    (world) ;; start with world
    (-log 'debug "initialized" dispatch)
    dispatch))


;; (time-delta -> percentage)
(define (scale-helper delta) (/ delta 100))

(define kDefaultLevelIndex 0)
(define kDefaultLevel "level-default")
(define kSpeedyLevel "level-speedy")
(define kSlowLevel "level-slow")
