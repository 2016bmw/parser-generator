(define (p:string string)
  (let ((str-len (string-length string)))
    (define (string-match data success)
      ;; (pp (list "string-match" string data))
      (and (>= (string-length data) str-len)
           (string=? string (substring data 0 str-len))
           (success (list string) str-len)))
    string-match))

(define (p:char-from string)
    (define (char-from-matcher data success)
      (let ((choices (string->list string))
	    (data-list (string->list data)))
	(and (not (null? data-list))
	     (memq (car data-list) choices)
	     (success (list (car (string->list data))) 1))))
    char-from-matcher)

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
                    (cons parse-tree cur-parse-tree)))))
            ((null? lst)
             (success (reverse cur-parse-tree) (- (string-length data) (string-length cur-data))))
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

(define (p:repeat matcher min max)
  (define (repeat-match data success)
    (let lp ((i 0)
             (cur-data data)
             (cur-parse-tree (list)))
      (if (and max (> i max))
          #f
          (or (and (>= i min)
                   (success cur-parse-tree (- (string-length data)
                                              (string-length cur-data))))
              (matcher cur-data
                       (lambda (parse-tree num-consumed)
                         ;; P:REPEAT has weird behavior when the base matcher
                         ;; matches an empty string.
                         ;; TODO clarify what the behavior *should* be.
			 (define (loop-again)
			   (lp (+ i 1)
			       (substring cur-data num-consumed (string-length cur-data))
			       (append cur-parse-tree (list parse-tree))))

			 (if (zero? num-consumed)
			     (if (string=? cur-data data) ; haven't consumed any part of original string
				 (if (>= (+ i 1) min)
				     (success (append cur-parse-tree (list parse-tree)) num-consumed)
				     (loop-again))
				 #f) ; never try to put an empty string after non-empty matches
			     (if (and (> i 0) (string=? cur-data data)) ; current match is not empty, but there has previously been at least one empty match
				 #f
				 (loop-again)))))))))
  repeat-match)
#|
                         (cond
                          ((zero? num-consumed)
                           (and (>= i min)
                                (success (append cur-parse-tree (list parse-tree)) num-consumed)))
                          ((positive? num-consumed)
                           (lp (+ i 1)
                               (substring cur-data num-consumed
                                          (string-length cur-data))
                               (append cur-parse-tree
                                       (list parse-tree))))
                          (else #f))))))))

  repeat-match)
|#

;;; Convenience combinators

(define (p:+ matcher)
  (p:repeat matcher 1 #f))

(define (p:? matcher)
  (p:repeat matcher 0 1))

(define (p:* matcher)
  (p:repeat matcher 0 #f))

;;; Definition for named rule

(define (p:rule name matcher)
  (define (rule-matcher data success)
    (matcher data
             (lambda (parse-tree num-consumed)
               (success (cons name parse-tree) num-consumed))))
  rule-matcher)

;; P:DELAYED takes a promise that will return a matcher, and returns a matcher
;; that just forces the promise and invokes the matcher returned from it. It
;; exists solely to break cyclic dependencies between grammar rules.
(define (p:delayed matcher)
  (define (delayed-matcher data success)
    ((force matcher) data success))
  delayed-matcher)

(define (p:transform f matcher)
  (define (transform-matcher data success)
    (matcher data
             (lambda (parse-tree num-consumed)
               (success (f parse-tree)
                        num-consumed))))
    transform-matcher)

;;; Tests

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


((p:repeat (p:choice (p:string "") (p:string "x")) 3 5) "xxxx" try-match)
;; ("parse-tree" (("") ("") ("")))
;; ("parse-tree" (("x") ("x") ("x")))
;; ("parse-tree" (("x") ("x") ("x") ("x")))
;; ;Value: #f

((p:repeat (p:choice (p:string "") (p:string "x")) 3 #f) "xxxx" try-match)
;; ("parse-tree" (("") ("") ("")))
;; ("parse-tree" (("x") ("x") ("x")))
;; ("parse-tree" (("x") ("x") ("x") ("x")))
;; ;Value: #f

((p:repeat (p:choice (p:string "") (p:string "x")) 0 #f) "xxxx" try-match)
;; ("parse-tree" ())
;; ("parse-tree" (("")))
;; ("parse-tree" (("x")))
;; ("parse-tree" (("x") ("x")))
;; ("parse-tree" (("x") ("x") ("x")))
;; ("parse-tree" (("x") ("x") ("x") ("x")))
;; ;Value: #f

((p:seq (p:repeat (p:choice (p:string "") (p:string "x")) 0 #f) (p:string "y")) "z" try-match)
;; ;Value: #f

((p:seq (p:repeat (p:choice (p:string "") (p:string "x")) 0 #f) (p:string "z")) "z" try-match)
("parse-tree" (() ("z")))
("parse-tree" ((("")) ("z")))
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

((p:char-from "abc") "abc" try-match)
;; ("parse-tree" (#\a))
;; ;Value: #f

((p:char-from "abc") "bbc" try-match)
;; ("parse-tree" (#\b))
;; ;Value: #f

((p:char-from "abc") "xbc" try-match)
;; ;Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:* (p:seq (p:choice (p:string "a") (p:string "b"))
			   (p:choice (p:string "c") (p:string "d")))))))
  (pattern "adadbc" try-match))
;; ("parse-tree" (test-rule))
;; ("parse-tree" (test-rule (("a") ("d"))))
;; ("parse-tree" (test-rule (("a") ("d")) (("a") ("d"))))
;; ("parse-tree" (test-rule (("a") ("d")) (("a") ("d")) (("b") ("c"))))
;; ;Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:+ (p:seq (p:choice (p:string "a") (p:string "b"))
			   (p:choice (p:string "c") (p:string "d")))))))
  (pattern "adadbc" try-match))
;; ("parse-tree" (test-rule (("a") ("d"))))
;; ("parse-tree" (test-rule (("a") ("d")) (("a") ("d"))))
;; ("parse-tree" (test-rule (("a") ("d")) (("a") ("d")) (("b") ("c"))))
;; ;Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:? (p:seq (p:choice (p:string "a") (p:string "b"))
			   (p:choice (p:string "c") (p:string "d")))))))
  (pattern "adadbc" try-match))
;; ("parse-tree" (test-rule))
;; ("parse-tree" (test-rule (("a") ("d"))))
;; ;Value: #f

|#
