#lang racket
(define (p) (q))

(define (test x y) (if (= x 0) 0 y))

(test 0 (p))
    
