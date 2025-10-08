#lang racket

; 1.(a) (5 points)
; Define a Racket procedure (change item1 item2 lst)
; that changes every top-level occurrence of item1 in lst to item2.
; Use only the built-in Racket special forms and procedures:
; define, lambda, null?, empty?, equal?, if, cond, car, first,
; cdr, rest, cons, and quoted constant lists like ’().
; Do not write any auxiliary procedures for this problem.

(define (change item1 item2 lst)
    (if (empty? lst) '()
        (if (equal? (car lst) item1)
            (cons item2 (change item1 item2 (cdr lst)))
            (cons (car lst) (change item1 item2 (cdr lst)))
        )
    )
)

(require racket/trace)

(trace change)

(change 3 8 '(1 3 4 3))