(load "primitives")

;;; Arithmetic Grammar Example

(define p:digit
  (p:choice
   (p:string "0")
   (p:string "1")
   (p:string "2")
   (p:string "3")
   (p:string "4")
   (p:string "5")
   (p:string "6")
   (p:string "7")
   (p:string "8")
   (p:string "9")))

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
