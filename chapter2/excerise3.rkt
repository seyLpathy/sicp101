#lang racket
(require "excerise2.rkt")
(require "../chapter1/excerises.rkt")
(define (enumerate-interval x y) (if (> x y) null (cons x (enumerate-interval (+ x 1) y))))
(enumerate-interval 1 8)
(define (make-pair n)
;debug 
(accumulate append null (map (lambda (i) (map (lambda (j) (list i j))
(enumerate-interval 1 (- i 1))))
(enumerate-interval 1 n))))
;(make-pair 8)
(define (flatmap proc seq)
(accumulate append null (map proc seq)))
;excerise 2.40
(define (make-pair-sum pair) (list (car pair) (cdr pair) (+ (car pair) (cdr pair))))
(define (prime-sum? x) (prime? (+ (car x) (cdr x))))
(define (prime-sum-pairs n)
    (map make-pair-sum (filter prime-sum? (make-pair n))))

;excerise 2.41
(define (order-triple-equality n s)
  (filter (lambda (x) (= s (accumulate + 0 x))) (make-triple n)))
  

;need to understanding only remove one level parenthess
(define (make-triple n)
          (accumulate append null 
            (map (lambda (i) 
                (accumulate append null
                    (map (lambda (j) 
                        (map (lambda (k) (list i j k))
                            (enumerate-interval 1 (- j 1))))      
                    (enumerate-interval 1 (- i 1)))))
            (enumerate-interval 1 n))))

;(make-triple 8)
;(order-triple-equality 8 10)


;excerise 2.42
(define empty-board null)

(define (adjoin-position new-row k rest-of-queens)
(cons (list new-row k) rest-of-queens)
)

(define (safe? k position)
    (define (iter new-pos rest)
        (or (null? rest)
            (and (not (let ((r (car new-pos)) (c (cadr new-pos)) (r2 (caar rest)) (c2 (cadar rest))) (or (= r r2) (= (abs (- r r2)) (abs (- c c2))))))
            (iter new-pos (cdr rest)))))
    (iter (car position) (cdr position))
)

(define (queens board-size)
    (define (queen-cols k)
    (display k)
    (newline )
        (if (= k 0) (list empty-board)
            (filter (lambda (positions) (safe? k positions))
            ; safe procedure works to filter out the positions that no fit the criteria
            ; line 61 to cons the kth proposed position to the rest k-1 cols which have been checked
                (flatmap (lambda (rest-of-queens) (map (lambda (new-row) (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size))) 
                (queen-cols (- k 1))))))
    (queen-cols board-size))



;(queens 6)
(define (queens-v1 board-size)
    (define (queen-cols-v1 k)
    (display k)
    (newline )
        (if (= k 0) (list empty-board)
            (filter (lambda (positions) (safe? k positions))
            ; safe procedure works to filter out the positions that no fit the criteria
            ; line 61 to cons the kth proposed position to the rest k-1 cols which have been checked
                (flatmap (lambda (new-row) (map (lambda (rest-of-queens) (adjoin-position new-row k rest-of-queens)) (queen-cols-v1 (- k 1))) 
                ) (enumerate-interval 1 board-size)))))
    (queen-cols-v1 board-size))

(queens-v1 6)
;excerise 2.43
;cause the interchage, it will slow board-size time
