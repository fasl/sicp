(define true #t)
(define false #f)
(define nil '())

;2.73
;.3.2節で記号微分を行うプログラムを述べた: 
;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        ((sum? exp)
;         (make-sum (deriv (addend exp) var)
;                   (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;           (make-product (multiplier exp)
;                         (deriv (multiplicand exp) var))
;           (make-product (deriv (multiplier exp) var)
;                         (multiplicand exp))))
;       ⟨更に多くの規則をここに追加出来る.⟩
;        (else (error "unknown expression type -- DERIV" exp))))
;このプログラムは微分する式の型による振分けを実行するプログラムと見ることが出来る. 
;その状況ではデータの「型タグ」は(+のような)代数演算の記号で, 実行する演算はderivである. 元の微分手続きを 
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;のように書き直し, このプログラムをデータ主導の形に変換出来る. 

;a. 上でやったことを説明せよ. 述語number?やvariable?がデータ主導の振分けに吸収出来ないのはなぜか. 

;b. 和と積の微分の手続きを書き, 上のプログラムで使う表に, それらを設定するのに必要な補助プログラムを書け. 
(define (operator make-sum) (deriv (addend exp) var))
(define (operands make-sum)  (deriv (augend exp) var))

;c. (問題2.56)のべき乗のような, その他の微分規則を選び, このデータ主導システムに設定せよ. 

;d. この代数式操作では, 式の型はそれを結合している代数演算である. しかし手続きの目印を反対にし, derivの振分けを 
;((get (operator exp) 'deriv) (operands exp) var)
;のようにしたとしよう. 微分システムには対応したどのような変更が必要か. 
