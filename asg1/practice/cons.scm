#lang racket

(define (iterator construct)
    (cond ((null? (cdr construct))
     (printf "~a~n" (car construct))
     (cons (car construct) null))
    (else
     (printf "~a~n" (car construct))
     (cons (car construct)
           (iterator (cdr construct))))))

(define (main)
    (printf "Hello World!\n")
    (iterator (cons 1 (cons 2 (cons 3 null))) ))

(main)