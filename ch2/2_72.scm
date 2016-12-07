(define true #t)
(define false #f)
(define nil '())

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object) (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree) (list (symbol-leaf tree)) (caddr tree)))

(define (weight tree)
  (if (leaf? tree) (weight-leaf tree) (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

 (define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; 記号
                               (cadr pair))  ; 頻度
                    (make-leaf-set (cdr pairs))))))

  
;符号化木と例題の通信文を定義する: 
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                  (make-leaf 'B 2)
                  (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Q2_68
 ;; ----------------------------------------------- 
 ;; EXERCISE 2.68 
 ;; ----------------------------------------------- 
  
 ;; helpers 
 (define (element-of-set? x set) 
   (cond ((null? set) false) 
         ((equal? x (car set)) true) 
         (else (element-of-set? x (cdr set))))) 
 (define (make-leaf symbol weight) 
   (list 'leaf symbol weight)) 
 (define (leaf? object) 
   (eq? (car object) 'leaf)) 
 (define (symbol-leaf x) (cadr x)) 
 (define (left-branch tree) (car tree)) 
 (define (right-branch tree) (cadr tree)) 
 (define (symbols tree) 
   (if (leaf? tree) 
       (list (symbol-leaf tree)) 
       (caddr tree))) 
 (define (weight-leaf x) (caddr x)) 
 (define (make-code-tree left right) 
   (list left 
         right 
         (append (symbols left) (symbols right)) 
         (+ (weight left) (weight right)))) 
 (define (weight tree) 
   (if (leaf? tree) 
       (weight-leaf tree) 
       (cadddr tree))) 
  
 (define (encode message tree) 
   (if (null? message) 
       '() 
       (append (encode-symbol (car message) tree) 
               (encode (cdr message) tree)))) 
  
 ;; solution 
 (define (encode-symbol smb tree) 
   (define (branch-correct? branch) 
     (if (leaf? branch) 
         (equal? smb (symbol-leaf branch)) 
         (element-of-set? smb (symbols branch)))) 
  
   (let ((lb (left-branch tree)) 
         (rb (right-branch tree))) 
     (cond ((branch-correct? lb) 
            (if (leaf? lb) '(0) (cons 0 (encode-symbol smb lb)))) 
           ((branch-correct? rb) 
            (if (leaf? rb) '(1) (cons 1 (encode-symbol smb rb)))) 
           (else (error "bad symbol -- ENCODE-SYMBOL" bit))))) 
  
; tests 
 (define sample-tree 
   (make-code-tree (make-leaf 'A 4) 
                   (make-code-tree 
                    (make-leaf 'B 2) 
                    (make-code-tree (make-leaf 'D 1) 
                                    (make-leaf 'C 1))))) 
;(print (encode '(A D A B B C A) sample-tree))
; (0 1 1 0 0 1 0 1 0 1 1 1 0) 

;Q68
;encode手続きは引数として通信文と木をとり, 符号化された通信文のビットのリストを作る. 

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;encode-symbolは自分で書く手続きで, 与えられた木に従って与えられた記号を符号化したビットのリストを返すものである. 
;encode-symbolの設計では, 記号が木になければ, エラーとしなければならない. 出来た手続きを問題2.67で得た結果と,
; 例題の木を使って符号化し, 元の例題の通信文と同じかどうかを見てテストせよ.