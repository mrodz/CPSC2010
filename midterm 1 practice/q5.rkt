#lang racket

(define (rep1 value n)
    (if (= n 0)
        (list value)
        (list (rep1 value (- n 1)) 
              (rep1 value (- n 1)))))

(define (rep2 value n)
    (if (= n 0)
        (list value)
        (let [(x (rep2 value (- n 1)))] (list x x))
    )
)