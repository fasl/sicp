(define true #t)
(define false #f)
(define nil '())

'http://stackoverflow.com/questions/40283810/how-to-implement-put-get-procedure-in-scheme
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)    
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation - TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

'http://d.hatena.ne.jp/awacio/20101106/1289052134
(use srfi-1) ;filter-mapの為
(define (install-polynomial-package)
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1)
	 (variable? v2)
	 (eq? v1 v2)))
  (define (make-term order coeff) (list order coeff))
  (define (order term) (car term))
  (define (coeff term) (cadr term))

  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (the-empty-termlist) '())
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
	(the-empty-termlist)
	(let ((t2 (first-term L)))
	  (adjoin-term
	   (make-term (+ (order t1) (order t2))
		      (mul (coeff t1) (coeff t2)))
	   (mul-term-by-all-terms t1 (rest-terms L))))))
	   
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
	term-list
	(cons term term-list)))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
	  ((empty-termlist? L2) L1)
	  (else
	   (let ((t1 (first-term L1))
		 (t2 (first-term L2)))
	     (cond ((> (order t1) (order t2))
		    (adjoin-term
		     t1 (add-terms (rest-terms L1) L2)))
		   ((< (order t1) (order t2))
		    (adjoin-term
		     t2 (add-terms L1 (rest-terms L2))))
		   (else
		    (adjoin-term
		     (make-term (order t1)
				(add (coeff t1) (coeff t2)))
		     (add-terms (rest-terms L1)
				(rest-terms L2)))))))))
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
	(the-empty-termlist)
	(add-terms (mul-term-by-all-terms (first-term L1) L2)
		   (mul-terms (rest-terms L1) L2))))


  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (add-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- ADD-POLY"
	       (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
	(make-poly (variable p1)
		   (mul-terms (term-list p1)
			      (term-list p2)))
	(error "Polys not in same var -- MUL-POLY"
	       (list p1 p2))))
  ;ゼロ確認
  (define (=zero-poly? polynomial)
    ;多項式の係数だけを=zero?チェックする手続き
    (define (coeff-list term-list)
      (filter-map (lambda (x)
		    (not (=zero? (coeff x))))
		  term-list))
    ;メインの処理
    (let ((terms (term-list polynomial)))
      (cond ((empty-termlist? terms)
	     #t)
	    ((null? (coeff-list terms))
	     #t)
	    (else
	     #f))))
  (define (tag p)
    (attach-tag 'polynomial p))

  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  (put '=zero? '(polynomial)
       (lambda (x) (=zero-poly? (contents x))))
  'done)
(install-polynomial-package)

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))
  
 (define (test-=zero?)
  (define (print-test polynomial answer)
    (let ((result (=zero? polynomial)))
      (print (eq? result answer) " : " polynomial)
      (print "    result = " result)
      (print "    answer = " answer)))

  (print-test '(polynomial x (2 1) (1 4) (0 2)) #f)
  (print-test '(polynomial x (2 0) (1 0) (0 2)) #f)
  (print-test '(polynomial x (2 0) (1 0) (0 0)) #t)
  (print-test '(polynomial x (2 0) ((rational 1 . 3) 0) (0 2)) #f)
  (print-test '(polynomial x (2 0) (0 (rational 1 . 3)) (0 2)) #f)
  (print-test '(polynomial x (2 0) (0 (rational 0 . 3)) (0 0)) #f)
  (print-test '(polynomial x
  			   (2 (complex rectangular 2 . 1))
  			   (1 (rational 1 . 3))
  			   (0 (real . 1.41421356)))
	      #f)
  (print-test '(polynomial x
  			   (2 (complex rectangular 0 . 0))
  			   (1 (rational 0 . 3))
  			   (0 (real . 0)))
	      #t)
  (print-test '(polynomial x
  			   (2 (complex rectangular 0 . 0))
  			   (1 (polynomial y (5 0) (3 0) (0 0)))
  			   (0 (real . 0)))
	      #t)
  )
 
 
(print (polynomial x (2 1) (1 4) (0 2)))
  
  
  