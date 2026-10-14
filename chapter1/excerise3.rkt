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

;excerise1.37
;a) recursive process: build the fraction outward from the innermost term
(define (cont-frac n d k)
    (define (frac i)
        (if (= i k)
            (/ (n i) (d i))
            (/ (n i) (+ (d i) (frac (+ i 1))))))
    (frac 1))

;b) iterative process: combine the terms from the innermost one upward
(define (cont-frac-iter n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (+ (d i) result)))))
    (iter (- k 1) (/ (n k) (d k))))

;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)
;=> 0.6180555555555556
;(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)
;=> 0.6180555555555556

;1/phi = 0.6180339887...
;k = 10 is off by ~5.6e-5, k = 11 by ~2.2e-5 -> k must be at least 11
;(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 10)
;(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 10)

;excerise1.38 using cont-frac to calculate the natural logarithms
(cont-frac-iter (lambda (i) 1) (lambda (i) (cond ((< i 3) i) ((< (remainder i 3) 2) 1)
((= (remainder i 3) 2) (* 2 (+ (/ (- i 2) 3) 1))) )) 50)

;excerise1.39 variations for de francitionibus continuis
;tan x = x / (1 - x^2/(3 - x^2/(5 - x^2/(7 - ...))))
;n(1) = x, n(i) = x^2 for i > 1; d(i) = 2i - 1; each layer subtracts the tail
(define (tan-cf x k)
    (define (n i) (if (= i 1) x (* x x)))
    (define (d i) (- (* 2 i) 1))
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1) (/ (n i) (- (d i) result)))))
    (iter (- k 1) (/ (n k) (d k))))

;(tan-cf 1 10)
;=> 1.5574077246549023 = tan(1)

;same thing on top of cont-frac, pushing the minus into the numerator
(define (tan-cf2 x k)
    (cont-frac (lambda (i) (if (= i 1) x (- (* x x))))
               (lambda (i) (- (* 2 i) 1))
               k))
(tan-cf2 1 10)


(define tolerance 0.00001)
(define (fixed-point f first-guess)
        (define (close-enough? v1 v2)
            (< (abs (- v1 v2)) tolerance))
        (define (try guess)
                (let ((next (f guess)))
                    (if (close-enough? guess next)
                        next
                        (try next))))
        (try first-guess))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess))

(define dx 0.0000001)
(define (deriv g) (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))


((deriv (lambda(i) (* i i i))) 5)

(define (cubic a b c)
    (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))
(newtons-method (cubic 0 0 -1) 1.0)
;cubic 0 0 -1

;excerise1.41
(define (double-v1 f) (lambda (x) (f (f x))) )
;(((double-v1 (double-v1 double-v1)) inc) 5)

;excerise 1.42
(define (compose f g) (lambda (x) (f (g x))))
(define (square x) (* x x))
;((compose square inc) 6)

;excerise 1.43
(define (repeat f n) (if (= n 0) (lambda (x) x) (compose f (repeat f (- n 1)))))
((repeat square 2) 5)

;excerise 1.44
(define (smooth f) (lambda (x) (/ 3 (+ (f (+ dx x)) (f x) (f (- x dx))))))
(define (n-fold f n) (repeat (smooth f) n))
;((n-fold square 10) 2)

;excerise 1.45
(define (average a b) (/ (+ a b) 2))
(define (average-damp f) (lambda (x) (average x (f x))))
(define (power b p) 
    (define (power-iter b p result) (if (= p 0) result (power-iter b (- p 1) (* result b))))
    (power-iter b p 1)
    )
; to calculate the 8th root ,test how many times of repeated is needed
;todo very impotant need further review
(define (root-8th x n)
(fixed-point ((repeat average-damp n) (lambda (y) (/ x (power y 7) )))  1.0))
(root-8th 256 3)

;excerise 1.46
;define iterative improvement
; (define (fixed-point f first-guess)
;         (define (close-enough? v1 v2)
;             (< (abs (- v1 v2)) tolerance))
;         (define (try guess)
;                 (let ((next (f guess)))
;                     (if (close-enough? guess next)
;                         next
;                         (try next))))
;         (try first-guess))
(define (iterative-improve f g) 
        (lambda (guess)
        (define (try guess)
                (let ((next (f guess)))
                    (if (g guess next)
                        next
                        (try next))))
        (try guess)))

(define (fixed-point-v1  f first-guess)
    ((iterative-improve f (lambda (old new) (< (abs (- old new)) tolerance)))
    first-guess))


