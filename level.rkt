#lang racket/base
(require "helpers.rkt"
         "logger.rkt"
         "range.rkt"
         "score.rkt"
         "velocity.rkt")
(provide MakeLevel MakeDefaultLevel MakeSpeedyLevel)


(define (MakeDefaultLevel #:log [-log (MakeLogger)])
  (let ((level (MakeLevel #:log -log)))
    (level 'name!                        "normal")
    (level 'friction!                    -10)
    (level 'gravity!                     (MakeVelocity 0 -0.98))
    (level 'player-mass!                 2)
    (level 'player-strafe!               20)
    (level 'player-jump!                 (MakeVelocity 0 70))
    (level 'player-bounce!               (MakeRange 10 30))
    (level 'obstacle-speed!              (MakeRange -20 -5))
    (level 'obstacle-size!               (MakeRange 50 200))
    (level 'obstacle-x!                  (MakeRange 200  600))
    (level 'obstacle-y!                  (MakeRange 0 300))
    (level 'obstacle-cleanup-offset!     -100)
    (level 'obstacle-generate-offset!    800)
    (level 'collectable-mass!            3)
    (level 'collectable-speed!           (MakeRange -50 -5))
    (level 'collectable-x!               (MakeRange 0 890))
    (level 'collectable-y!               (MakeRange 600 2400))
    (level 'collectable-cleanup-offset!  -100)
    (level 'collectable-generate-offset! 0)
    level))


(define (MakeSpeedyLevel #:log [-log (MakeLogger)])
  (let ((level (MakeLevel #:log -log)))
    (level 'name!                        "hard")
    (level 'friction!                    -5)
    (level 'gravity!                     (MakeVelocity 0 -0.94))
    (level 'player-mass!                 10)
    (level 'player-strafe!               30)
    (level 'player-jump!                 (MakeVelocity 0 170))
    (level 'player-bounce!               (MakeRange 10 30))
    (level 'player-bounce!               (MakeRange 30 60))
    (level 'obstacle-speed!              (MakeRange -50 -20))
    (level 'obstacle-size!               (MakeRange 50 200))
    (level 'obstacle-x!                  (MakeRange 100  700))
    (level 'obstacle-y!                  (MakeRange 0 300))
    (level 'obstacle-cleanup-offset!     -100)
    (level 'obstacle-generate-offset!    800)
    (level 'collectable-mass!            5)
    (level 'collectable-speed!           (MakeRange 10 200))
    (level 'collectable-x!               (MakeRange 100  700))
    (level 'collectable-y!               (MakeRange -50 0))
    (level 'collectable-cleanup-offset!  -100)
    (level 'collectable-generate-offset! 0)
    level))


(define (MakeLevel #:log [-log (MakeLogger)])
  (let ((-name      "default")
        (-friction  0)
        (-gravity   (MakeVelocity 0 0 #:log -log))
        (-pmass     0)
        (-pstrafe   0)
        (-pjump     (MakeVelocity 0 0 #:log -log))
        (-pbounce   (MakeRange 0 0 #:log -log))
        (-ospeed    (MakeRange 0 0 #:log -log))
        (-osize     (MakeRange 0 0 #:log -log))
        (-ox        (MakeRange 0 0 #:log -log))
        (-oy        (MakeRange 0 0 #:log -log))
        (-ocleanup  0)
        (-ogenerate 0)
        (-cmass     0)
        (-cspeed    (MakeRange 0 0 #:log -log))
        (-cx        (MakeRange 0 0 #:log -log))
        (-cy        (MakeRange 0 0 #:log -log))
        (-ccleanup  0)
        (-cgenerate 0)
        (-score     (MakeScore #:log -log)))
    (define (Level msg . args)
      (case msg
        ((score)                        -score)
        ((name)                         -name)
        ((friction)                     -friction)
        ((gravity)                      -gravity)
        ((player-mass)                  -pmass)
        ((player-strafe)                -pstrafe)
        ((player-jump)                  -pjump)
        ((player-bounce)                -pbounce)
        ((obstacle-speed)               -ospeed)
        ((obstacle-size)                -osize)
        ((obstacle-x)                   -ox)
        ((obstacle-y)                   -oy)
        ((obstacle-cleanup-offset)      -ocleanup)
        ((obstacle-generate-offset)     -ogenerate)
        ((collectable-mass)             -cmass)
        ((collectable-speed)            -cspeed)
        ((collectable-x)                -cx)
        ((collectable-y)                -cy)
        ((collectable-cleanup-offset)   -ccleanup)
        ((collectable-generate-offset)  -cgenerate)
        ((player-path)                  (apply player-path args))
        ((name!)                        (apply set-name! args))
        ((friction!)                    (apply set-friction! args))
        ((gravity!)                     (apply set-gravity! args))
        ((player-mass!)                 (apply set-pmass! args))
        ((player-strafe!)               (apply set-pstrafe! args))
        ((player-jump!)                 (apply set-pjump! args))
        ((player-bounce!)               (apply set-pbounce! args))
        ((obstacle-speed!)              (apply set-ospeed! args))
        ((obstacle-size!)               (apply set-osize! args))
        ((obstacle-x!)                  (apply set-ox! args))
        ((obstacle-y!)                  (apply set-oy! args))
        ((obstacle-cleanup-offset!)     (apply set-ocleanup! args))
        ((obstacle-generate-offset!)    (apply set-ogenerate! args))
        ((collectable-mass!)            (apply set-cmass! args))
        ((collectable-speed!)           (apply set-cspeed! args))
        ((collectable-x!)               (apply set-cx! args))
        ((collectable-y!)               (apply set-cy! args))
        ((collectable-cleanup-offset!)  (apply set-ccleanup! args))
        ((collectable-generate-offset!) (apply set-cgenerate! args))
        ((encode)                       (apply encode args))
        ((decode)                       (apply decode args))
        (else
          (-log 'fatal "method missing" msg dispatch))))
    (define dispatch Level)

    (define (player-path idx ref)
      (cond
        ((and (eq? idx 1) (eq? ref 'a)) kPlayerPath1a)
        ((and (eq? idx 1) (eq? ref 'b)) kPlayerPath1b)
        ((and (eq? idx 1) (eq? ref 'c)) kPlayerPath1c)
        ((and (eq? idx 2) (eq? ref 'a)) kPlayerPath2a)
        ((and (eq? idx 2) (eq? ref 'b)) kPlayerPath2b)
        ((and (eq? idx 2) (eq? ref 'c)) kPlayerPath2c)
        (else
          (-log 'warn "missing player-path id" dispatch))))

    (define (set-name! name)           (set! -name name))
    (define (set-friction! friction)   (set! -friction friction))
    (define (set-gravity! gravity)     (set! -gravity gravity))
    (define (set-pmass! pmass)         (set! -pmass pmass))
    (define (set-pstrafe! pstrafe)     (set! -pstrafe pstrafe))
    (define (set-pjump! pjump)         (set! -pjump pjump))
    (define (set-pbounce! pbounce)     (set! -pbounce pbounce))
    (define (set-ospeed! ospeed)       (set! -ospeed ospeed))
    (define (set-osize! osize)         (set! -osize osize))
    (define (set-ox! ox)               (set! -ox ox))
    (define (set-oy! oy)               (set! -oy oy))
    (define (set-ocleanup! ocleanup)   (set! -ocleanup ocleanup))
    (define (set-ogenerate! ogenerate) (set! -ogenerate ogenerate))
    (define (set-cmass! cmass)         (set! -cmass cmass))
    (define (set-cspeed! cspeed)       (set! -cspeed cspeed))
    (define (set-cx! cx)               (set! -cx cx))
    (define (set-cy! cy)               (set! -cy cy))
    (define (set-ccleanup! ccleanup)   (set! -ccleanup ccleanup))
    (define (set-cgenerate! cgenerate) (set! -cgenerate cgenerate))

    (define (encode) ;; Serialize object to a Vector
      (vector 'level
              (cons 'score (-score 'encode))
              (cons 'name -name)
              (cons 'friction -friction)
              (cons 'gravity (-gravity 'encode))
              (cons 'player-mass -pmass)
              (cons 'player-strafe -pstrafe)
              (cons 'player-jump (-pjump 'encode))
              (cons 'player-bounce (-pbounce 'encode))
              (cons 'obstacle-speed (-ospeed 'encode))
              (cons 'obstacle-size (-osize 'encode))
              (cons 'obstacle-x (-ox 'encode))
              (cons 'obstacle-y (-oy 'encode))
              (cons 'obstacle-cleanup -ocleanup)
              (cons 'obstacle-generate -ogenerate)
              (cons 'collectable-mass -cmass)
              (cons 'collectable-speed (-cspeed 'encode))
              (cons 'collectable-x (-cx 'encode))
              (cons 'collectable-y (-cy 'encode))
              (cons 'collectable-cleanup -ccleanup)
              (cons 'collectable-generate -cgenerate)))

    (define (decode vect) ;; Deserialize object from a Vector
      (unless (eq?      (vector-ref vect 0) 'level) (-log 'warn "wrong encoded vector for" dispatch))
      (-score 'decode   (cdr (vector-ref vect 1)))
      (set-name!        (cdr (vector-ref vect 2)))
      (set-friction!    (cdr (vector-ref vect 3)))
      (-gravity 'decode (cdr (vector-ref vect 4)))
      (set-pmass!       (cdr (vector-ref vect 5)))
      (set-pstrafe!     (cdr (vector-ref vect 6)))
      (-pjump 'decode   (cdr (vector-ref vect 7)))
      (-pbounce 'decode (cdr (vector-ref vect 8)))
      (-ospeed 'decode  (cdr (vector-ref vect 9)))
      (-osize 'decode   (cdr (vector-ref vect 10)))
      (-ox 'decode      (cdr (vector-ref vect 11)))
      (-oy 'decode      (cdr (vector-ref vect 12)))
      (set-ocleanup!    (cdr (vector-ref vect 13)))
      (set-ogenerate!   (cdr (vector-ref vect 14)))
      (set-cmass!       (cdr (vector-ref vect 15)))
      (-cspeed 'decode  (cdr (vector-ref vect 16)))
      (-cx 'decode      (cdr (vector-ref vect 17)))
      (-cy 'decode      (cdr (vector-ref vect 18)))
      (set-ccleanup!    (cdr (vector-ref vect 19)))
      (set-cgenerate!   (cdr (vector-ref vect 20))))

    ;; Private
    (-log 'debug "initialized" dispatch)
    dispatch))


(define kNullNumber   0)
(define kNullRange    (MakeRange 0 0))
(define kNullVelocity (MakeVelocity 0 0))
(define kPlayerPath1a "resources/player1a.png")
(define kPlayerPath1b "resources/player1b.png")
(define kPlayerPath1c "resources/player1c.png")
(define kPlayerPath2a "resources/player2a.png")
(define kPlayerPath2b "resources/player2b.png")
(define kPlayerPath2c "resources/player2b.png")