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
  
;問題 2.65 2.3.3章
;問題2.63と2.64の結果を使い, (釣合った)二進木として実装されている集合のunion-setとintersection-setをTheta(n)で実装せよ

;順序有りリスト用
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

;順序ありリスト用　2.62より　
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((= (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) (cdr set2))))
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1) set2)))
        (else (cons (car set2) (union-set set1 (cdr set2))))))	
		
(define test-tree1 '(7 (3 (1 () ()) (5 () ())) (11 (9 () ()) (14 () ()))))
(define test-tree2 '(8 (4 (2 () ()) (6 () ())) (12 (10 () ()) (14 () ()))))

(define list1 (tree->list-1 test-tree1))
(define list2 (tree->list-1 test-tree2))
(define union-list (union-set list1 list2))  
(define intersection-list (intersection-set list1 list2))
(print union-list)
(print intersection-list)