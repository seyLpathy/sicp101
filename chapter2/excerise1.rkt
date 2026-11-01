#lang racket
(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* ( denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* ( denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y)) (* (numer y) (denom x))))

(define (gcd x y)
    (if (= y 0) x (gcd y (remainder x y) )))

(define (make-rat n d) (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
        (newline)
        (display (numer x))
        (display "/")
        (display (denom x)))

(define one-half (make-rat 1 2))

(define (print-point p)
(newline)
(display "(")
(display (x-point p))
(display ",")
(display (y-point p))
(display ")"))
;excerise2.2
(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))
(define (make-segment start end) (cons start end))
(define (start-seg p) (car p))
(define (end-seg p) (cdr p))
(define (print-segment segment) 
    (print-point (start-seg segment))
    (print-point (end-seg segment)))

(define four-fifth (make-point 4 5))
(define two-third (make-point 2 3))
(print-segment (make-segment four-fifth two-third))
(define (mid-point segment) (make-point (/ (+ (x-point (start-seg segment)) (x-point (end-seg segment))) 2)  (/ (+ (y-point (start-seg segment)) (y-point (end-seg segment))) 2)))

(print-point (mid-point (make-segment four-fifth two-third)))
;excerise2.3
;method 1 
;using two points to define a rectangle
;first parameter is top-right point,the second parameter is bottom-left point
;as long as you using two points of diagonal 
(define (rectangle p q) (cons p q))
(define (top-right r) (car r))
(define (bottom-left r) (cdr r))
(define (length r) (abs (- (x-point (top-right r)) (x-point (bottom-left r)))))
(define (width r) (abs (- (y-point (top-right r)) (y-point (bottom-left r)))))
(define (get-area s) (* (length s) (width s)))
(define (get-perimeter s) (* 2 (+ (length s) (width s))))
(get-perimeter (rectangle (make-point 6 8) (make-point 2 4)))

;abstracted rational number presentations
(define (cons-1 x y)
    (define (dispatch m)
        (cond ((= m 0) x)
                ((= m 1) y)
                    (else (error "Argument not 0 or 1 -- CONS" m))))
                dispatch)
(define (car1 z) (z 0))
(define (cdr1 z) (z 1))

;excerise2.4 another way to represent the cons/cdr/car
(define (cons-2 x y)
        (lambda (m) (m x y)))
(define (car2 z)
        (z (lambda (p q) p)))
(define (cdr2 z)
        (z (lambda (p q) q)))
;(car2 (cons-2 2 5))
;(cdr2 (cons-2 2 5))

;excerise2.5 make rational numbers with arithmetic operations and number
(define (cons3 x y)
    (* (expt 2 x) (expt 3 y)))
(define (car3 z)
    (define (power-2 number result) (if (> (remainder number 2) 0) result (power-2 (/ number 2) (+ result 1))))
    (power-2 z 0))

(define (cdr3 z)
    (define (power-3 number result) (if (> (remainder number 3) 0) result (power-3 (/ number 3) (+ result 1))))
    (power-3 z 0))
(car3 (cons3 2 5))
(cdr3 (cons3 2 5))

;excerise2.6
;how many times f is used to calculate represents the number itself
(define zero (lambda (f) (lambda (x) x)))
;zero means not f works on it 
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define (add-one n)
        (lambda (f) (lambda (x) (f ((n f) x)))))
(define (plus a b)
    (lambda (f)
        (lambda (x) ((a f) ((b f) x)))))
;there is no pure existence only how you define it


;excerise2.7
;alyssa's method 
(define (add-interval x y)
(make-interval (+ (lower-bound x) (lower-bound y))
(+ (upper-bound x) (upper-bound y))))

(define (div-interval x y)
(mul-interval x (make-interval (/ 1.0 (upper-bound y))
(/ 1.0 (lower-bound y)))))

(define (div-interval-v1 x y)
  (if (and (< (lower-bound y) 0) (> (upper-bound y) 0)) 
    (error "div-interval: divisor interval spans zero") 
(mul-interval x (make-interval (/ 1.0 (upper-bound y))
(/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))
(define (mul-interval x y)
(let ((p1 (* (lower-bound x) (lower-bound y)))
(p2 (* (lower-bound x) (upper-bound y)))
(p3 (* (upper-bound x) (lower-bound y)))
(p4 (* (upper-bound x) (upper-bound y))))
(make-interval (min p1 p2 p3 p4)
(max p1 p2 p3 p4))))
(define (upper-bound x) (max (car x) (cdr x)))
(define (lower-bound x) (min (car x) (cdr x)))

;excerirse2.8
(define (sub-interval a b) 
    (add-interval a (make-interval (- (upper-bound b)) (- (lower-bound) b)))
)
;excerise2.9 width加减运算与参数本身的width相关,而乘除不相关

(define (result-width x) (/ (- (upper-bound x) (lower-bound x)) 2))
;纯粹数学问题 w1=(u1-p1)/2,w2=(u2-p2)/2 w3=w1+w2 (add)
;by defintion ,sub intervals equal to add (only change the sign)
;mul or devide,the upper-bound need to calculate with lower-bound,which can not be abstracted by definition of width 

;excerise2.10
;if the interval divided spans zero,there is no meaning for work

;excerise2.11
;create cases for the multi-interval
(define (make-center-width c w)
        (make-interval (- c w) (+ c w)))
(define (center i)
        (/ (+ (lower-bound i) (upper-bound i)) 2))
;excerise2.13
;define a selector percent that produces the percentage tolerance for a given interval
(define (make-center-percent center percent) (make-interval (* center (+ 1 percent)) (* center (- 1 percent))))
(define (percent i) (/ (- (upper-bound i) (center i)) (center i)))
(define a (make-center-percent 10 0.05))
(define b (make-center-percent 6  0.08))
;(percent (make-center-percent 10 0.1))
;the product of two interval defined by excerise2.12
(define c (mul-interval a b))
;(percent c)
(define (estimate-tolerance v1 v2) 
   (/ (+ (percent v1) (percent v2)) (+ 1 (* (percent v1) (* (percent v2))))))
;(estimate-tolerance a b)

;possible reason for the different answer for the computing of parallel risistance

(define (par1 r1 r2)
(div-interval (mul-interval r1 r2)
(add-interval r1 r2)))
(define (par2 r1 r2)
(let ((one (make-interval 1 1)))
(div-interval one
(add-interval (div-interval one r1)
(div-interval one r2)))))
(par1 a b)
(par2 a b)
;compute par1 a/a
(par1 a a)
(par2 a a)
(par1 b b)
(par2 b b)
;by data demostration, par2's tolerance will repeat and won't expand

;excerise2.16
;todo need  mapping it out with much time,good question

;excerise2.17
(define (list-length item)
        (define (length-iterative x count)
        (if(null? x) count (length-iterative (cdr x) (+ count 1))))
        (length-iterative item 0))
(define l1 (list 1 2 3 4 5 6))
(list-length l1)
(define (last-item item)
        (if (= (list-length item) 1) item (last-item (cdr item))))
;(last-item l1)
;excerise2.18
;reverse the list
l1
(define (append l1 l2)
        (if (null? l1) l2 (cons (car l1) (append (cdr l1) l2))))
(define (reverse-list item)
        (if (null? item) '() (append (reverse-list (cdr item)) (list  (car item)))))
(reverse-list l1)
;excerise2.19 
;todo need further working 

;excerise2.20 
;Use this notation to write a procedure same-parity that takes one or more integers and returns a list of all the arguments that have the same even-odd parity as the first argument.
;review this is a serious error,need thinking more
;选出所有奇偶性与第一个元素相同的元素
(define (same-parity first . rest)
(define (same? n)
(if (even? first)
    (even? n)
    (odd? n)))
(cons first (filter same? rest)))
;(same-parity 2 4 5 6 7 8 9 10)

;excerise 2.21
(define (square-list items)
(if (null? items)
items
(cons (* (car items) (car items)) (square-list (cdr items)))))
(define (square-list-map items)
(map (lambda (x) (* x x)) items))
;(square-list (list 1 2 3 4 5))
;(square-list-map (list 1 2 3 4 5))
;how would define map in racket 

;iteratively mapping 
(define (square-list-iter items)
    (define (aid items result)
        (if (null? items) (reverse result) (aid (cdr items) (cons (* (car items) (car items)) result))))
        (aid items null)
        )

;(square-list-iter (list 1 2 3 4 5))

;implementation of for-each
(define (for-each-self proc items)
    (if (null? items)
    #t
    (begin (proc (car items))
            (for-each-self proc (cdr items)))
    )
)
;hirarchical structure
