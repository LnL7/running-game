#lang racket/base
(require "../lib/canvas.rkt")
(require "queue.rkt"
         "point.rkt")


(let ((mdtr Mediator))
  (mdtr 'prefix 'Queue 'NoMethod 'FullCollection 'EmptyCollection)
  (mdtr 'prefix 'Point 'NoMethod)

  (mdtr 'on 'Point.Render put-pixel!)

  (let ((lg (Logger 'new mdtr)))
    (lg 'NoMethod 'Fatal)
    (lg 'FullCollection 'Warn)
    (lg 'EmptyCollection 'Warn)
    (void)))


(define (r rmin rmax) (+ (random (- rmax rmin -1)) rmin))

(define generate   0.5)
(define move-speed 0.0005)
(define max-speed  30)
(define speed      0.001)
(define pos-x      400)
(define pos-y      300)
(define color      (find-color "red"))
(define store (Queue 'new (expt 2 8) (vector)))

(define (fill! ctr)
  (cond
    ((< ctr 0)      'done)
    ((store 'full?) (store 'pop!) (fill! (- ctr 1)))
    (else
      (store 'push!
             (vector
               (let ((pos  (Point 'new pos-x pos-y))) pos)
               (let ((vel  (Point 'new (r (- max-speed) (+ max-speed)) (r (- max-speed) (+ max-speed))))) vel)))
      (fill! (- ctr 1)))))

(start-game-loop
  (lambda (dt)
    (store 'each
           (lambda (e)
             (let ((pos  (vector-ref e 0))
                   (vel  (vector-ref e 1)))
               (pos 'x! (+ (pos 'x) (* (vel 'x) dt speed)))
               (pos 'y! (+ (pos 'y) (* (vel 'y) dt speed)))
               (pos 'render! color)
               (set! pos-x (+ pos-x (* dt move-speed)))
               (cond
                 ((< pos-x 0)                  (set! move-speed (+ (abs move-speed))))
                 ((> pos-x (get-canvas-width)) (set! move-speed (- (abs move-speed))))))))
    (fill! (* dt generate)))
  #t)
