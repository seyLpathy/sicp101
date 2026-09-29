#lang racket
(provide (all-defined-out))
;; newton's iteration 
(define tolerance 0.0001)

(define (average a b) (/ (+ a b) 2))

(define (good-enough guess x)
    (< (abs (- (* guess guess) x)) tolerance))

(define (next-guess guess x) 
    (average guess (/ x guess)))

(define (sqrt-iter guess x)
    (if (good-enough guess x)
    guess
    (sqrt-iter (next-guess guess x) x)))

(sqrt-iter 3.0 2)