(load "primitives")
(load "test-primitives")

;;; Add more primitive to deal with whitespaces

(define (p:seq-skip-whitespace . args)
  (let* ((whitespace-matcher
	  (p:* (p:char-predicate char-whitespace?)))
	 ;; Ignore whitespace in between
	 (whitespace-delimited-matcher
	  (apply p:separated-by
		 (cons whitespace-matcher (cons #f args))))
	 ;; Ignore whitespace at beginning and end
	 (ignore-whitespace-matcher
	  (p:seq whitespace-matcher
		 whitespace-delimited-matcher
		 whitespace-matcher)))
    ;; Only take the non-whitespace part
    (p:transform
     get-tail-transformer
     ignore-whitespace-matcher)))	

(define (p:*-skip-whitespace matcher)
  (p:* (p:transform get-head-transformer
		    (p:seq-skip-whitespace matcher))))


;;; Grammar Rules

#|
number   ::= [0-9]+
var      ::= [a-zA-Z]+
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

(define unnest-tail-transformer
  (lambda (parse-tree)
    (let ((data (parse-tree-data parse-tree)))
      (cons (car data) (cadr data)))))

(define get-head-transformer
  (lambda (parse-tree)
    (car (parse-tree-data parse-tree))))

(define get-tail-transformer
  (lambda (parse-tree)
    (cadr (parse-tree-data parse-tree))))

(define product-rule
  (p:rule 'product
	  (p:transform
	   unnest-tail-transformer
	   (p:seq-skip-whitespace
	    atom-matcher
	    (p:*-skip-whitespace
	     (p:transform
	      get-tail-transformer
	      (p:seq-skip-whitespace
	       (p:string "*") atom-matcher)))))))

(define sum-rule
  (p:rule 'sum
	  (p:transform
	   unnest-tail-transformer
	   (p:seq-skip-whitespace
	    product-rule
	    (p:*-skip-whitespace
	     (p:transform
	      get-tail-transformer
	      (p:seq-skip-whitespace
	       (p:string "+") product-rule)))))))

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
(parse-expression " 1  *   2 * 2  * ( 3+4+5)+x")
;; Value 41: (sum (product (number . 1) (number . 2) (number
;; . 2) (sum (product (number . 3)) (product (number
;; . 4)) (product (number . 5)))) (product (var . x)))
(pp (parse-expression "1 + 2 * (3 + x) + ((4)) * 5"))
;; (sum (product (number . 1))
;;      (product (number . 2) (sum (product (number . 3)) (product (var . x))))
;;      (product (sum (product (sum (product (number . 4))))) (number . 5)))
|#
(simplify (parse-expression "1 + 2 * (3 + x) + ((4)) * 5") '())

(define (expr-tree-rule expr-tree) (car expr-tree))
(define (expr-tree-value expr-tree) (cdr expr-tree))

(define (number-node? expr-tree)
  (eq? 'number (expr-tree-rule expr-tree)))
(define (var-node? expr-tree)
  (eq? 'var (expr-tree-rule expr-tree)))
(define (product-node? expr-tree)
  (eq? 'product (expr-tree-rule expr-tree)))
(define (sum-node? expr-tree)
  (eq? 'sum (expr-tree-rule expr-tree)))

(define (simplify-number expr-tree env) expr-tree)

(define (simplify-var expr-tree env)
  (let* ((var (expr-tree-value expr-tree))
	 (env-entry (assq var env)))
    (if (pair? env-entry)
	(cons 'number (cadr env-entry))
	expr-tree)))

(define (simplify-sum expr-tree env)
  (let ((simplified-parts
	 (map (lambda (expr) (simplify expr env))
	      (expr-tree-value expr-tree))))
    (if (every number-node? simplified-parts)
	(cons 'number
	      (apply + (map expr-tree-value simplified-parts)))
	;; If there's just one argument, bring it out of sum
	(if (= 1 (length simplified-parts))
	    (car simplified-parts)
	    (cons 'sum simplified-parts)))))
    
(define (simplify-product expr-tree env)
  (let ((simplified-parts
	 (map (lambda (expr) (simplify expr env))
	      (expr-tree-value expr-tree))))
    (if (every number-node? simplified-parts)
	(cons 'number
	      (apply * (map expr-tree-value simplified-parts)))
	;; If there's just one argument, bring it out of product
	(if (= 1 (length simplified-parts))
	    (car simplified-parts)
	    (cons 'product simplified-parts)))))

(define (simplify expr-tree env)
  (cond ((number-node? expr-tree)
	 (simplify-number expr-tree env))
	((var-node? expr-tree)
	 (simplify-var expr-tree env))
	((product-node? expr-tree)
	 (simplify-product expr-tree env))
	((sum-node? expr-tree)
	 (simplify-sum expr-tree env))))

#|

;;; Test simplify

(simplify (parse-expression "1") '())
;Value 48: (number . 1)

(simplify (parse-expression "x") '((x 4)))
;Value 49: (number . 4)

(simplify (parse-expression "y") '((x 4)))
;Value 50: (var . y)

(simplify (parse-expression "1 + 2") '((x 4)))
;Value 57: (number . 3)

(simplify (parse-expression "2 * 3") '((x 4)))
;Value 58: (number . 6)

(simplify (parse-expression "1 + 2 * 3 + 4") '((x 4)))
;Value 59: (number . 11)

(simplify (parse-expression "1 + x * 3 + 4") '((x 4)))
;Value 62: (number . 17)

(simplify (parse-expression "1 + 2 * 3 + x") '((x 4)))
;Value 59: (number . 11)

(simplify (parse-expression "1 + x*3 + y") '((x 4)))
;Value 65: (sum (number . 1) (number . 12) (var . y))

(simplify (parse-expression "1 + x*3*(y + 3*(1 + x)) + 4*(((z)))")
	  '((x 4) (y 5) (z 6)))
;Value 70: (number . 265)

(simplify (parse-expression "1 + x*3*(y + 3*(1 + x)) + 4*(((z)))")
	  '((x 4) (z 6)))
;Value 71: (sum (number . 1) (product (number . 4) (number . 3) (sum (var . y) (number . 15))) (number . 24))

(simplify (parse-expression "1 + 2 * (3 + x) + ((4)) * 5") '())

|#
