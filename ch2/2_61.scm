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
	  
(define (adjoin-set x set)
  (cond ((and (null? x) (null? set)) '())
        ((null? set) cons x '())
		((not (pair? set)) (adjoin-set x (car set)))
        ((null? x) set)
		((= x (car set)) set)
        ((< x (car set)) (cons x set))
        (else  (cons (car set) (adjoin-set x (cdr set))))))

(define (adjoin-set3 x set)
     cons x set)
		
(define odds '(1 3 5 7))
(define evens '(2 4 6 8))
(define primes '(2 3 5 7))

(print evens)
(print (adjoin-set 1 evens))
(print (adjoin-set 9 evens))
(print (adjoin-set 5 evens))

