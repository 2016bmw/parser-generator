(define (p:string string)
  (let ((str-len (string-length string)))
    (define (string-match data success)
      ;; (pp (list "string-match" string data))
      (and (>= (string-length data) str-len)
           (string=? string (substring data 0 str-len))
           (success (list string) str-len)))
    string-match))


(define (p:seq . args)
  (define (seq-match data success)
    (let lp ((lst args)
             (cur-data data)
             (cur-parse-tree (list)))
      ;; (pp (list "data " cur-data))
      (cond ((pair? lst)
             ((car lst) cur-data
              (lambda (parse-tree num-consumed)
                ;; (pp parse-tree)
                ;; (pp num-consumed)
                (lp (cdr lst)
                    (substring cur-data num-consumed (string-length cur-data))
                    (append cur-parse-tree (list parse-tree))))))
            ((null? lst)
             (success cur-parse-tree (- (string-length data) (string-length cur-data))))
            (else (error "Should not get here!")))))
  seq-match)

(define (p:choice . args)
  (define (choice-match data success)
    (let lp ((lst args))
      (cond ((pair? lst)
             (or ((car lst) data success)
                 (lp (cdr lst))))
            ((null? lst) #f)
            (else (error "Should not get here (p:choice)")))))
  choice-match)

(define (p:repeat pattern min max)
  (define (repeat-match data success)
    (let lp ((i 0)
             (cur-data data)
             (cur-parse-tree (list)))
      (if (and max (> i max))
          #f
          (or (and (>= i min)
                   (success cur-parse-tree (- (string-length data)
                                              (string-length cur-data))))
              (pattern cur-data
                       (lambda (parse-tree num-consumed)
                         ;; TODO: every repeat has to consume a
                         ;; character. FIX THIS.
                         (and (> num-consumed 0)
                              (lp (+ i 1)
                                  (substring cur-data num-consumed
                                             (string-length cur-data))
                                  (append cur-parse-tree
                                          (list parse-tree))))))))))
  repeat-match)


(define (p:rule name matcher)
  (define (rule-matcher data success)
    (matcher data
             (lambda (parse-tree num-consumed)
               (success (cons name parse-tree) num-consumed))))
  rule-matcher)

;;; Tesst

(define (try-match parse-tree num-consumed)
  (pp (list "parse-tree" parse-tree))
  #f)

#|

((p:string "a") "a" try-match)
("parse-tree" ("a"))
;Value: #f                              ;

((p:string "a") "ab" try-match)
("parse-tree" ("a"))
;Value: #f                              ;

((p:string "abc") "ab" try-match)
;Value: #f                              ;

((p:string "b") "ab" try-match)
;Value: #f                              ;

((p:seq (p:string "hello") (p:string "kitty")) "hellokitty" try-match)
;("parse-tree" (("hello") ("kitty")))   ;
;Value: #f                              ;

((p:choice (p:string "hello") (p:string "kitty")) "hello" try-match)
("parse-tree" ("hello"))
;Value #f                               ;

((p:repeat (p:string "a") 1 1) "a" try-match)
;; ("parse-tree" (("a")))

((p:repeat (p:string "a") 1 3) "aa" try-match)
;; ("parse-tree" (("a")))
;; ("parse-tree" (("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 1 3) "aaaa" try-match)
;; ("parse-tree" (("a")))
;; ("parse-tree" (("a") ("a")))
;; ("parse-tree" (("a") ("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 1 3) "aaba" try-match)
;; ("parse-tree" (("a")))
;; ("parse-tree" (("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 3 4) "aaba" try-match)
;Value: #f                              ;

((p:repeat (p:string "a") 1 #f) "aaba" try-match)
;; ("parse-tree" (("a")))
;; ("parse-tree" (("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "kitty") 1 #f) "kittykittykittykittyhellokitty" try-match)
;; ("parse-tree" (("kitty")))
;; ("parse-tree" (("kitty") ("kitty")))
;; ("parse-tree" (("kitty") ("kitty") ("kitty")))
;; ("parse-tree" (("kitty") ("kitty") ("kitty") ("kitty")))
;; ;Value: #f

(let ((pattern
       (p:repeat (p:seq (p:choice (p:string "a") (p:string "b"))
                        (p:choice (p:string "c") (p:string "d")))
                 3 #f)))
  (pattern "acadbcbc" try-match))
;; ("parse-tree" ((("a") ("c")) (("a") ("d")) (("b") ("c"))))
;; ("parse-tree" ((("a") ("c")) (("a") ("d")) (("b") ("c")) (("b") ("c"))))
;; ;Value: #f

(let ((pattern
       (p:repeat (p:seq (p:choice (p:string "a") (p:string "b"))
                        (p:choice (p:string "c") (p:string "d")))
                 3 #f)))
  (pattern "cacadbcbc" try-match))
;; Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:repeat (p:seq (p:choice (p:string "a") (p:string "b"))
                                (p:choice (p:string "c") (p:string "d")))
                         3 #f))))
  (pattern "acadbcbc" try-match))
;; ("parse-tree" (test-rule (("a") ("c")) (("a") ("d")) (("b") ("c"))))
;; ("parse-tree"
;;  (test-rule (("a") ("c")) (("a") ("d")) (("b") ("c")) (("b") ("c"))))
;; ;Value: #f

|#
