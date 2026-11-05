#lang racket
(define (memq-v1 item x)
    (cond ((null? x) false)
          ((eq? item (car x)) x)
          (else (memq-v1 item (cdr x))))

    )

(memq-v1 'apple '(pear banana prune))
(memq-v1 'apple '(x (apple sauce) y apple pear))

;excerise 2.53
(list 'a 'b 'c)
;equal to '(a b c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;excerise 2.54
;recursively equal judgement
;(define (recursive-equal x y))