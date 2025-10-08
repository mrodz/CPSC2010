#lang racket

; 3. (a) (6 points) Define a recursive Racket procedure
; (numbers lst) that takes an nb-list lst and returns a list of all
; the numbers that occur in lst, in the left-to-right
; order in which they occur. Preserve duplicates.
; Use only the built-in Racket special forms and procedures:
; define, lambda, null?, empty?, symbol?, boolean?, number?,
; if, cond, car, first, cdr, rest, cons, append, and quoted
; constant lists like ’().
; Do not write any auxiliary procedures for this problem.
; Examples:
; (numbers '(#f 1 (2 ()) ((1)))) => '(1 2 1)
; (numbers '(#t 2 ((#f)) 26 #f (3 2))) => '(2 26 3 2)

(define (numbers lst)
    (if (list? lst)
        (if (empty? lst)
            '()
            (append (numbers (car lst)) (numbers (cdr lst)))
        )
        (if (number? lst)
            (cons lst '())
            '()
        )
    )
)

(require racket/trace)

(trace numbers)

(numbers '((#t 1) (4 5)))