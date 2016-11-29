(define true #t)
(define false #f)
(define nil '())

;問題 2.61 2.3.3章
;順序づけられた表現を使った adjoin-setの実装を示せ. element-of-set?の類推で, 
;順序づけられない表現に比べ, 約半分のステップ数を必要とする手続きを作るのに, 順序の利点をどう用いるか示せ. 

;順序なし
(define (element-of-set2? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set2? x (cdr set)))))
		
(define (adjoin-set2 x set)
  (if (element-of-set2? x set)
      set
      (cons x set)))
	  
;順序あり
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
;順序なし
(define (union-set2 set1 set2) 
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2) (union-set2 (cdr set1) set2))
        (else (cons (car set1) (union-set2 (cdr set1) set2)))))
		
(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()    
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((< x2 x1)
               (intersection-set set1 (cdr set2)))))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))			   

;letでは変数が1つしか取れない?  intersection-setではなぜOK?
(define (union-set3 set1 set2) 
  (print (car set2))
  (cond ((and (null? set1) (null? set2)) '())
		((null? set1) set2)
        ((null? set2) set1)
		((not (pair? set1)) (cons set1 set2))
		((not (pair? set2)) (cons set1 set2))
		(let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2)
					(cons x1 (union-set3 (cdr set1) (cdr set2))))
				 ((< x1 x2)
					(cons x1 (union-set3 (cdr set1) set2)))
			     ((> x1 x2)
					(cons x2 (union-set3 set1 (cdr set2))))))))
		
(define odds '(1 3 5 7))
(define odds2 '(1 3 5 7 8))
(define evens '(2 4 6 8))
(define primes '(2 3 5 7))

(print evens)
;(print (intersection-set evens odds2))
(print (union-set evens evens))
(print (union-set evens odds2))
(print (union-set odds evens))

;(print (adjoin-set 5 evens))

