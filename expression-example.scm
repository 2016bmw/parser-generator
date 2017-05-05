(load "primitives")

;;; Grammar Rules

#|

atom     ::= number | var | '(' sum ')'
product  ::= atom ('*' atom)*
sum      ::= product ('*' product)*

|#

(define number-rule
  (p:rule 'number
          (p:transform
           (lambda (parse-tree)
	     (let* ((data (parse-tree-data parse-tree))
		    (string (apply string-append (apply append data))))
               (string->number string)))
           (p:+ (p:char-predicate char-numeric?)))))

(define var-rule
  (p:rule 'var
	  (p:transform
           (lambda (parse-tree)
	     (let* ((data (parse-tree-data parse-tree))
		    (string (apply string-append (apply append data))))
               (symbol string)))
           (p:+ (p:char-predicate char-alphabetic?)))))

;; Match parenthesized sum-rule and then strip the parens
(define parenthesized-matcher
  (p:transform
   (lambda (parse-tree)
     (cadr (parse-tree-data parse-tree)))
   (p:seq (p:string "(")
	  (p:delayed (delay sum-rule))
	  (p:string ")"))))

(define atom-matcher
   (p:choice number-rule var-rule parenthesized-matcher))

(define atom-repeat-transformer
  (lambda (parse-tree)
    (let ((data (parse-tree-data parse-tree)))
      (cons (car data) (cadr data)))))

(define ignore-head-transformer
  (lambda (parse-tree)
    (cadr (parse-tree-data parse-tree))))

(define product-rule
  (p:rule 'product
	  (p:transform
	   atom-repeat-transformer
	   (p:seq atom-matcher
		  (p:*
		   (p:transform
		    ignore-head-transformer
		    (p:seq (p:string "*") atom-matcher)))))))

(define sum-rule
  (p:rule 'sum
	  (p:transform
	   atom-repeat-transformer
	   (p:seq product-rule
		  (p:*
		   (p:transform
		    ignore-head-transformer
		    (p:seq (p:string "+") product-rule)))))))

;;; Public API

(define expression-parser
  (lambda (expression success)
    (sum-rule
     expression
     (lambda (parse-tree num-consumed)
       (and (= num-consumed (string-length expression))
	    (success parse-tree))))))

(define (parse-expression expression)
  (call-with-current-continuation
   (lambda (cc) (expression-parser expression cc))))

#|

(parse-expression "sdfsdf  sdfd")
;; Value: #f
(parse-expression "1*2*2*(3+4+5)+x")
;; Value 22: (sum (product (number . 1) (number . 2) (number . 2) (sum
;; (product (number . 3)) (product (number . 4)) (product (number
;; . 5)))) (product (var . x)))

|#


