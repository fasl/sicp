
;#!/usr/local/bin/gosh

(use www.cgi)
(use srfi-19)
(use text.tree)
(use text.html-lite)

(define nil '())

;(define (square-list items)
; (if (null? items)
;  ()
; (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (enumerate-interval low high)
 (if (> low high)
  '()
 (cons low (enumerate-interval (+ low 1) high))))

;(print (enumerate-interval 2 7))

;(accumulate append
;            nil
;            (map (lambda (i)
;                   (map (lambda (j) (list i j))
;                        (enumerate-interval 1 (- i 1))))
;                 (enumerate-interval 1 n)))
				 
(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
  
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
  
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
      (filter prime-sum?
        (flatmap
         (lambda (i)
           (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
         (enumerate-interval 1 n)))))
 
;(print (prime-sum-pairs 6))

(define (queens board-size)
  (define (queen-cols k)  
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1)))))))

(define (adjoin-position new-row k rest-of-queens)
   
)		  
		  
(print (queens 5))
		  
		  
