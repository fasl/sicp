;#!/usr/local/bin/gosh

(define nil '())

(define right-split (split beside below))
(define up-split (split below beside))

(define (right-split2 painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split2 painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (up-split2 painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split2 painter (- n 1))))
        (below painter (beside smaller smaller)))))	
		
(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))
		
(define (split ope1 ope2)
  (lambda (spliter painter n)
     (if (= n 0)
      painter
      (let ((smaller (spliter painter (- n 1))))
        (ope1 painter (ope2 smaller smaller)))))
  )

		