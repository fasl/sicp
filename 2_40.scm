
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

(define (accumulate append 
            nil
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 n))))
				 
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
  
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
		 
 (define (prime-sum-pairs2 n) 
   (map make-sum-pair 
        (filter prime-sum? (unique-pairs n)))) 

;(print (prime-sum-pairs 6))
;(print (prime-sum-pairs2 6))

;(define (unique-pair n)
;   (enumerate-interval 1 n)
;    )
	
(define (unique-pairs n)
  (flatmap
    (lambda (i)
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(print (prime-sum-pairs 6))
	
;(print (unique-pair 3))
;(print (enumerate-interval 1 5))
