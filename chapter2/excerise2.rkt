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
;(define x (list 1 2 3))
;(define y (list 4 5 6))
;(cons x y)
;(append x y)
;(list x y)
;deep reverse implementations
; append (list x) equals to list ..x
(define x  (list (list 1 2) (list 3 4)))
(reverse x)
(define (deep-reverse x) (cond
                                ((null? x) x)
                                ((not(pair? x)) x)
                                (else (append (deep-reverse (cdr x)) (list (deep-reverse (car x)))))))
(deep-reverse x)

(define (fringe x)
  (cond 
    ((null? x) x)
    ((not (pair? x)) (list x))
    (else (append (fringe (car x)) (fringe (cdr x))))
    )
)
(fringe x)

;2.30 square tree
(define (square-tree x) (cond ((null? x) x) ((not (pair? x)) (* x x)) (else (cons (square-tree (car x)) (square-tree (cdr x))))))
(square-tree x)
; implements square tree by mapping,fuction works on elements of tree
(define (square-tree-map x) (map (lambda (x) (if (not (pair? x)) (* x x) (square-tree-map x) )) x))
;(square-tree-map x)
(define (tree-map f x) (cond ((null? x) x) ((not (pair? x)) (f x)) (else (cons (tree-map f (car x)) (tree-map f (cdr x))))))
(tree-map (lambda (x) (* x x)) x)

;2.32
(define (subsets s)
(if (null? s)
(list s)
(let ((rest (subsets (cdr s))))
(append rest (map (lambda (x) (cons (car s) x)) rest)))))
(define z (list 1 2 3))
(subsets z)

;2.33 accumulate to implements basic list operation
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
;(define (map p sequence)
;(accumulate (lambda (x y) (cons (p x) y)) null sequence))
;(define (append seq1 seq2)
;(accumulate cons seq2 seq1))
;(define (length sequence)
;(accumulate (lambda (x y) (+ 1 y)) 0 sequence))
;a higher level of abstraction of procedure
;todo need further standing
;2.34 hornel's rule
(define (horner-eval x coefficient-sequence)
(accumulate (lambda (this-coeff higher-terms) (+ (* this-coeff x) higher-terms))
0
coefficient-sequence))

;2.35
(define (count-leave-ac x)
    (accumulate (lambda (x y) (cond ((null? x) y) ((not (pair? x)) (+ 1 y)) (else (+ (count-leave-ac x) y)))) 0 x))
(count-leave-ac x)

;ex 2.36 sequencial accumulation
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        null
        (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))
        ))
(define t (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
;(accumulate-n + 0 t)
;需确认racket中map的完整定义
(define (dot-product v w)
(accumulate + 0 (map * v w)))
(define (matrix-*-vector m v)
(map (lambda (x) (dot-product x v)) m))
(define (transpose mat)
(accumulate-n cons null mat))
(define (matrix-*-matrix m n)
(let ((cols (transpose n)))
(map (lambda (x) (matrix-*-vector cols x)) m)))
(define A (list (list 1 2) (list 3 4)))
(define B (list (list 1 2) (list 3 4)))
(dot-product (list 1 2) (list 1 2))
(matrix-*-vector A (list 1 2))
(transpose A)
(matrix-*-matrix A B)

;ex 2.38
;right-fold and left fold 
(define (fold-left op initial sequence)
(define (iter result rest)
(if (null? rest)
result
(iter (op result (car rest))
(cdr rest))))
(iter initial sequence))
;(accumulate / 1 (list 1 2 3))
;(fold-left / 1 (list 1 2 3))
(accumulate list null (list 1 2 3))
(fold-left list null (list 1 2 3))
(accumulate * 1 (list 1 2 3))
(fold-left * 1 (list 1 2 3))
;满足交换律的算子才能实现左折叠等价于右折叠

;excerise 2.39
;left-fold and right-fold to implement the reverse function
(define (reverse-right sequence)
(accumulate (lambda (x y) (append y (list x))) null sequence))
(reverse-right x)
(define (reverse-left sequence)
(fold-left (lambda (x y) (cons y x)) null sequence))
(reverse-left x)
