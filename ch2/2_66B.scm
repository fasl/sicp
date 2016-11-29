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
(define (key record)
  (car record))

(define (value record)
  (cadr record))

(define (make-record key value)
  (list key value))

(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((entry-of-record (entry set-of-records)))
           (let ((key-of-record (key entry-of-record)))
                (cond ((= given-key key-of-record) (value entry-of-record))
                      ((< given-key key-of-record) (lookup given-key (left-branch set-of-records)))
                      ((> given-key key-of-record) (lookup given-key (right-branch set-of-records))))))))

(define record
  (list->tree (list
                (make-record 1 'john)
                (make-record 2 'paul)
                (make-record 3 'george)
                (make-record 4 'ringo))))
; ((2 paul) ((1 john) () ()) ((3 george) () ((4 ringo) () ())))

(print (lookup 1 record))