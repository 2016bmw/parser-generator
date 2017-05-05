(load "primitives")

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
;; ("parse-tree" ("a"))
;; ;Value: #f

((p:char-from "abc") "bbc" try-match)
;; ("parse-tree" ("b"))
;; ;Value: #f

((p:char-from "abc") "xbc" try-match)
;; ;Value: #f

((p:char-not-from "abc") "abc" try-match)
;; ;Value: #f

((p:char-not-from "abc") "bbc" try-match)
;; ;Value: #f

((p:char-not-from "abc") "xbc" try-match)
;; ("parse-tree" ("x"))
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

((p:require
  (lambda (parse-tree)
    (> (string-length (car parse-tree)) 3))
  (p:choice (p:string "h") (p:string "he") (p:string "hello")
	    (p:string "hello kitty")))
 "hello kitty :)"
 try-match)
;; ("parse-tree" ("hello"))
;; ("parse-tree" ("hello kitty"))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #f (p:string "a") (p:string "b") (p:string "c")) "a    b        c" try-match)
;; ("parse-tree" (("a") ("b") ("c")))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #t (p:string "a") (p:string "b") (p:string "c")) "a    b        c" try-match)
;; ("parse-tree" (("a") ((" ") (" ") (" ") (" ")) ("b") ((" ") (" ") (" ") (" ") (" ") (" ") (" ") (" ")) ("c")))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #f (p:string "a") (p:string "b") (p:string "c")) "abc" try-match)
;; ("parse-tree" (("a") ("b") ("c")))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #t (p:string "a") (p:string "b") (p:string "c")) "abc" try-match)
;; ("parse-tree" (("a") () ("b") () ("c")))
;; ;Value: #f

|#

