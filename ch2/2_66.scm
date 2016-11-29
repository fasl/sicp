(define true #t)
(define false #f)
(define nil '())

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))
  
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  
  (define (copy-to-list tree result-list)
     (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))

;順序付けでないリストのlookup	nのオーダー				 
(define (lookup2 given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        (else (lookup2 given-key (cdr set-of-records)))))

;問題 2.66 2.3.3章
;レコードの集合がキーの数値で順序づけられている二進木で構造化されている場合のlookup手続きを実装せよ
(define (lookup given-key tree)
  (cond ((null? (car tree)) false)
        ((equal? given-key (entry tree)) true) 
        (cond ((< given-key (car tree)) lookup given-key right-branch)
		      (lookup given-key left-branch))))
  		
(print (lookup 5 '(7 (3 (1 () ()) (5 () ())) (11 (9 () ()) (14 () ())))))