#lang racket
(define test (list (list 1 2) 3 4))
(length (list test test))
;the basic length functions only count how many time it can apply car to the arguments
;count leaves
(define (count-leave x)
    (cond ((null? x) 0)
            ((not (pair? x)) 1)
            (else (+ (count-leave (car x)) (count-leave (cdr x))))
            ))

;(define x (list 1 (list 2 (list 3
;4))))
;(count-leave x)
(define x (list 1 2 3))
(define y (list 4 5 6))
(cons x y)
(append x y)
(list x y)
