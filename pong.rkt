#lang racket

(require 2htdp/image)
(require 2htdp/universe)
(require test-engine/racket-tests)

;                         paddle1     paddle2       ball        ball motion
; A World is a [ListOf (cons x1 y1) (cons x2 y2) (cons x y) (cons xdir ydir)]

(define blank-world
  (list (cons 40 300) (cons 760 300) (cons 400 300) (cons (random 2) (random 2))))

; speed of ball
(define speed 4)

; draw-world : World -> Image
; Renders world-state.
(define (draw-world w)
  (cond
    [(and (equal? (car (last w)) 0) (< (car (third w)) 32)) (place-image/align (text "Player 2 wins!" 50 "green") 400 300 "center" "center"
                       (place-image/align (rectangle 20 120 "solid" "white") (car (first w))  (cdr (first w))  "center" "center"
                       (place-image/align (rectangle 20 120 "solid" "white") (car (second w)) (cdr (second w)) "center" "center"
                       (place-image/align (rectangle 16 16 "solid" "white")  (car (third w))  (cdr (third w))  "center" "center"
                       (scene+line (empty-scene 800 600 "black") 400 0 400 800 "white")))))]
    [(and (equal? (car (last w)) 1) (> (car (third w)) 768)) (place-image/align (text "Player 1 wins!" 50 "green") 400 300 "center" "center"
                       (place-image/align (rectangle 20 120 "solid" "white") (car (first w))  (cdr (first w))  "center" "center"
                       (place-image/align (rectangle 20 120 "solid" "white") (car (second w)) (cdr (second w)) "center" "center"
                       (place-image/align (rectangle 16 16 "solid" "white")  (car (third w))  (cdr (third w))  "center" "center"
                       (scene+line (empty-scene 800 600 "black") 400 0 400 800 "white")))))]
    [else (place-image/align (rectangle 20 120 "solid" "white") (car (first w))  (cdr (first w))  "center" "center"
          (place-image/align (rectangle 20 120 "solid" "white") (car (second w)) (cdr (second w)) "center" "center"
          (place-image/align (rectangle 16 16 "solid" "white")  (car (third w))  (cdr (third w))  "center" "center"
          (scene+line (empty-scene 800 600 "black") 400 0 400 800 "white"))))]))

; draw : World-state, NewWorld-state -> NewWorld-state
; Replaces old World-state with new one.
(define (draw x y)
  (map (lambda (x) (cons (first x) (second x))) y))

(check-expect (draw 1 '((1 2) (3 4) (5 6))) '((1 . 2) (3 . 4) (5 . 6)))
(check-expect (draw 1 '((1 2 232) (3 4 82 19) (5 6 123 1))) '((1 . 2) (3 . 4) (5 . 6)))

; ball : World -> World
; Updates ball position.
(define (ball w)
  (cond
  [(and (equal? (car (last w)) 0) (< (car (third w)) 32)) w]
  [(and (equal? (car (last w)) 1) (> (car (third w)) 768)) w]
  [(and (>= (+ (cdr (car w)) 60) (cdr (third w))) (<= (- (cdr (car w)) 60) (cdr (third w))) (<= (car (third w)) 58))
     (if (equal? (last w) '(0 . 1))
       (list (first w) (second w) (cons (+ (car (third w)) speed) (+ (cdr (third w)) speed)) (cons 1 1))
       (list (first w) (second w) (cons (+ (car (third w)) speed) (- (cdr (third w)) speed)) (cons 1 0)))]
  [(and (>= (+ (cdr (second w)) 60) (cdr (third w))) (<= (- (cdr (second w)) 60) (cdr (third w))) (>= (car (third w)) 743))
     (if (equal? (last w) '(1 . 1))
       (list (first w) (second w) (cons (- (car (third w)) speed) (+ (cdr (third w)) speed)) (cons 0 1))
       (list (first w) (second w) (cons (- (car (third w)) speed) (- (cdr (third w)) speed)) (cons 0 0)))]
  [else 
  (if (or (equal? (cdr (third w)) 592)  (equal? (cdr (third w)) 8))
  (cond
    [(equal? (last w) '(1 . 0)) (list (first w) (second w) (cons (+ (car (third w)) speed) (+ (cdr (third w)) speed)) (cons 1 1))]
    [(equal? (last w) '(0 . 0)) (list (first w) (second w) (cons (- (car (third w)) speed) (+ (cdr (third w)) speed)) (cons 0 1))]
    [(equal? (last w) '(1 . 1)) (list (first w) (second w) (cons (+ (car (third w)) speed) (- (cdr (third w)) speed)) (cons 1 0))]
    [(equal? (last w) '(0 . 1)) (list (first w) (second w) (cons (- (car (third w)) speed) (- (cdr (third w)) speed)) (cons 0 0))])
  (cond
    [(equal? (last w) '(1 . 0)) (list (first w) (second w) (cons (+ (car (third w)) speed) (- (cdr (third w)) speed)) (last w))] ; up right
    [(equal? (last w) '(0 . 0)) (list (first w) (second w) (cons (- (car (third w)) speed) (- (cdr (third w)) speed)) (last w))] ; up left
    [(equal? (last w) '(1 . 1)) (list (first w) (second w) (cons (+ (car (third w)) speed) (+ (cdr (third w)) speed)) (last w))] ; down right
    [(equal? (last w) '(0 . 1)) (list (first w) (second w) (cons (- (car (third w)) speed) (+ (cdr (third w)) speed)) (last w))]))])) ; down left

(check-expect (ball '((40 . 300) (760 . 300) (400 . 300) (0 . 1))) '((40 . 300) (760 . 300) (396 . 304) (0 . 1)))
(check-expect (ball '((40 . 300) (760 . 300) (200 . 100) (1 . 1))) '((40 . 300) (760 . 300) (204 . 104) (1 . 1)))

; ballx : Universe -> Bundle
; Produces bundle with updated ball state.
(define (ball-universe x)
  (let ([new-world (ball (car x))])
    (make-bundle (cons new-world (cdr x))
               (map (lambda (i) (make-mail i (map (λ (y)
                                                    (if (pair? y)
                                                        (list (car y) (cdr y)) y)) new-world))) (cdr x))
               empty)))

; new-client : Universe IWorld -> Universe
; Adds the new client to our list of worlds.
(define (new-client u i)
  (cons (car u) (append (cdr u) (list i))))

; on-message : Universe IWorld Message -> Bundle
; Decides whether to move paddles and broadcasts new world to all the clients.
(define (on-message u from msg)
(let ([newworld
 (cond
    [(and (equal? from (first (cdr u))) (equal? msg "w") (>= (cdr (car (car u))) 70))
     (list (cons (car (car (car u))) (- (cdr (car (car u))) 15)) (second (car u)) (third (car u)) (last (car u)))]
    [(and (equal? from (first (cdr u))) (equal? msg "s") (<= (cdr (car (car u))) 530))
     (list (cons (car (car (car u))) (+ (cdr (car (car u))) 15)) (second (car u)) (third (car u)) (last (car u)))]
    [(and (equal? from (second (cdr u))) (equal? msg "up") (>= (cdr (second (car u))) 70))
     (list (car (car u)) (cons (car (second (car u))) (- (cdr (second (car u))) 15)) (third (car u)) (last (car u)))]
    [(and (equal? from (second (cdr u))) (equal? msg "down") (<= (cdr (second (car u))) 530)) 
     (list (car (car u)) (cons (car (second (car u))) (+ (cdr (second (car u))) 15)) (third (car u)) (last (car u)))]
    [else (car u)]) ])
  (make-bundle (cons newworld (cdr u))
               (map (lambda (i) (make-mail i (map (λ (x)
                                                    (if (pair? x)
                                                        (list (car x) (cdr x)) x)) newworld))) (cdr u))
               empty)))
    
(launch-many-worlds
  (universe (cons blank-world empty)
            [on-new new-client]
            [on-msg on-message]
            [on-tick ball-universe])
  (big-bang blank-world
            [to-draw    draw-world]
            [on-key     make-package]
            [on-receive draw]
            [name "Pong - Player 1"]
            [register LOCALHOST])
  (big-bang blank-world
            [to-draw    draw-world]
            [on-key     make-package]
            [on-receive draw]
            [name "Pong - Player2"]
            [register LOCALHOST]))
(test)
