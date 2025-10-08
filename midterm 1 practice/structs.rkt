#lang racket

(provide point line good-point? line)

(struct point (x y) #:transparent)

(define (good-point? point)
    (and (point? point) (number? (point-x point)) (number? (point-y point))))

(define (line p1 p2)
    (if (and (good-point? p1) (good-point? p2)) 
    (let [(x (- (point-x p2) (point-x p1))) (y (- (point-y p2) (point-y p1)))]
        (sqrt (+ (* x x) (* y y)))
    )
    'bad-points
))

(define p1 (point 0 0))
(define p2 (point 3 4))

(struct person (name age) #:transparent)