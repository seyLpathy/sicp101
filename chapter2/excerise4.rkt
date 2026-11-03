#lang racket
(require sicp-pict)
(define einstein2 (beside einstein (flip-vert einstein)))
;(paint einstein2)
;right-split painter
(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
            (beside painter (below smaller smaller)))))
;excerise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((up (up-split painter (- n 1))))
            (below painter (beside up up)))))
;(paint (right-split einstein 5))
;(paint (up-split einstein 5))


(define (corner-split painter n)
(if (= n 0)
painter
(let ((up (up-split painter (- n 1)))
(right (right-split painter (- n 1))))
(let ((top-left (beside up up))
(bottom-right (below right right))
(corner (corner-split painter (- n 1))))
(beside (below painter top-left)
(below bottom-right corner))))))
(paint (corner-split einstein 5))


(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
;two level abstraction
;one the square pattern tl br bl br
;second every piece of the square is also a pattern

