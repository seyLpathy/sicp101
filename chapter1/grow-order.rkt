#lang racket
(define (even? x) (= (remainder x 2) 0))

(define (fast-exponent b n)
    (display "this function has been called\n")
    (cond ((= n 0) 1)
            ((even? n) (square (fast-exponent b (/ n 2))))
            (else (* b (fast-exponent b (- n 1))))))

(define (square x) (* x x))
(fast-exponent 2 16)
;-------
;logorithm growth 
;-------
(define (fast-exponent-v2 b n product)
    (display "this function has been called\n")
    (cond ((= n 0) product )
            ((even? n) (fast-exponent-v2 (square b) (/ n 2) product))
            (else (fast-exponent-v2 b (- n 1) (* product b)))))
;(fast-exponent-v2 2 16 1)
;excerise 1.17
;implements addition with double,halve and addition
(define (double x) (* 2 x))
(define (halve x) (/ x 2))
;halve function only works with even numbers
(define (new-add a b)
    (cond ((= 0 b) a)
        ((even? b) (new-add (+ a (halve b)) (halve b)))
        (else (new-add (+ a 1) (- b 1)))))
(define (new-add-v1 a b addition)
    (cond ((= 0 b) addition)
        ((even? b) (new-add-v1 a (halve b) (+ addition (halve b))))
        (else (new-add-v1 a (- b 1) (+ addition 1)))))
;(new-add 5 15)
;(new-add-v1 5 15 0)

;super fibonacci algorithm
(define (fast-fib n)
    (fib-v2 1 0 0 1 n))
;equal to  fast-exponent a b n
(define (fib-v2 a b p q count)
    (cond ((= count 0) b)
        ((even? count) (fib-v2 a b (+ (* p q) (* q q)) (+ (* 2 p q) (* q q)) (/ count 2)))
        (else (fib-v2 (+ (* b q) (* a q) (* a p)) (+ (* b q) (* a q)) p q (- count 1)))))
(fast-fib 5)
