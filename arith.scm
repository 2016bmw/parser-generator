(load "primitives")
(load "test-primitives")

;;; Arithmetic Grammar Example

(define p:digit
  (p:char-predicate char-numeric?))

(define number-rule
  (p:rule 'number
          (p:transform
           (lambda (digits)
             (let ((string (apply string-append (apply append digits))))
               (string->number string)))
           (p:repeat p:digit 1 #f))))

(define arithmetic-grammar
  (letrec* ((addend-rule
             (p:choice number-rule
                       (p:seq (p:string "(")
                              (p:delayed (delay expr-rule))
                              (p:string ")"))))
            (add-rule
             (p:rule 'sum
                     (p:seq addend-rule
                            (p:repeat (p:seq (p:string "+") addend-rule)
                                      0 #f))))
            (expr-rule (p:rule 'expr add-rule)))
    expr-rule))

;; Tests

#|
(arithmetic-grammar "1+2" try-match)
;; ("parse-tree" (expr sum (number . 1) ()))
;; ("parse-tree" (expr sum (number . 1) ((("+") (number . 2)))))
;; ;Value: #f
|#
