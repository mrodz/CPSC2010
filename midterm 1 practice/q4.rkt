#lang racket

(require "tm.rkt")

(define tm 
    (list
        (ins 'q1 1 'q1 1 'R)
        (ins 'q1 'b 'q2 'E 'L)
        (ins 'q2 1 'q2 1 'L)
        (ins 'q2 'b 'q3 'b 'R)
        (ins 'q3 1 'q4 'S 'R)
        (ins 'q3 'E 'q7 1 'L)
        (ins 'q4 1 'q4 1 'R)
        (ins 'q4 'E 'q5 'E 'R)
        (ins 'q5 1 'q5 1 'R)
        (ins 'q5 'b 'q6 1 'L)
        (ins 'q6 1 'q6 1 'L)
        (ins 'q6 'E 'q6 'E 'L)
        (ins 'q6 'S 'q3 1 'R)
        (ins 'q7 1 'q7 1 'L)
        (ins 'q7 'b 'q8 'b 'R)
        (ins 'q8 1 'exit 'b 'R)
    ))

(simulate tm (conf 'q1 '() 1 '(1 1)) 200)