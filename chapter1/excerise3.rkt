#lang racket
(define (sum-v1 term a next b)
    (if (= a b) (term a)
    (+ (term a) (sum-v1 term (next a) next b))))

(define (identify x) x)
(define (cube x) (* x x x))
(define (inc x) (+ x 1))

(define (sumall a b)
    (sum-v1 identify a inc b))
(sumall 1 10)

(define (sumcube a b)
    (sum-v1 cube a inc b))
(sumcube 1 10)

;excerise 1.29
;this question tell you how to disinteger bigger problem to smaller problem 

(define (even? x) (= (remainder x 2) 0))
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (y k) (f (+ a (* h k))))
  (define (weight k)
        (cond 
        ((or (= k 0) (= k n)) 1)
        ((even? k) 2)
        (else 4)
        ))
  (define (term k) (* (weight k) ( y k)))
  ;using define that make constant values out of recursion calling functions
  (* (/ h 3) (sum-v1 term 0 inc n))
)

(simpson cube 0 1 100)
;excerise 1.30
(define (sum-v2 term a next b)
(define (iter a result)
(if (> a b) result  (iter (next a) (+ result (term a)))))
(iter a 0)
)
;inner function work as variable storage 
;(sum-v2 cube 1 inc 10)


;1.31 define high-order abstraction as product
;iterative process
(define (product f a next b)
    (define (iter-multi a result)
        (if (> a b) result (iter-multi (next a) (* result (f a)))))
        (iter-multi a 1)
        )
(product identify 1 inc 6)

;1.31
;this function only works for integer bigger than 2
(define (f1 x) (if (even? x) (/ x (+ x 1)) (/ (+ x 1) x)))
(define (cal-pi n)
    (* 4 (product f1 2 inc n)))

(cal-pi 100)


;1.32 abstract the common part of product or multiply
(define(accumulate combiner null-value term a next b) 
    (if (> a b) null-value (accumulate combiner  (combiner null-value (term a)) term (next a) next b)))
(accumulate * 1 identify 1 inc 6)
(accumulate + 0 identify 1 inc 10)

;1.33 based on 1.32 to add filter function
(define(accumulate-filter combiner filter null-value term a next b) 
    (if (> a b) null-value (accumulate-filter combiner filter (if (filter a) (combiner null-value (term a)) null-value) term (next a) next b)))
(accumulate * 1 identify 1 inc 6)
(define (even x) (= (remainder x 2) 0))
(accumulate-filter + even 0 identify 1 inc 10)




