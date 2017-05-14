(load "tree")
(load "primitives")

;;; Tests

(define (try-match parse-tree num-consumed)
  (pp (list "parse-tree" (parse-tree-text parse-tree)
	    (parse-tree-data parse-tree)))
  #f)

((p:string "a") "a" try-match)
;; ("parse-tree" "a" ("a"))
;Value: #f                              ;

((p:string "a") "ab" try-match)
;; ("parse-tree" "a" ("a"))
;Value: #f                              ;

((p:string "abc") "ab" try-match)
;Value: #f                              ;

((p:string "b") "ab" try-match)
;Value: #f                              ;

((p:seq (p:string "hello") (p:string "kitty")) "hellokitty" try-match)
;; ("parse-tree" "hellokitty" (("hello") ("kitty")))
;Value: #f                              ;

((p:choice (p:string "hello") (p:string "kitty")) "hello" try-match)
; ("parse-tree" "hello" ("hello"))
;Value #f                               ;

((p:repeat (p:string "a") 1 1) "a" try-match)
;; ("parse-tree" "a" (("a")))

((p:repeat (p:string "a") 1 3) "aa" try-match)
;; ("parse-tree" "a" (("a")))
;; ("parse-tree" "aa" (("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 1 3) "aaaa" try-match)
;; ("parse-tree" "a" (("a")))
;; ("parse-tree" "aa" (("a") ("a")))
;; ("parse-tree" "aaa" (("a") ("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 1 3) "aaba" try-match)
;; ("parse-tree" "a" (("a")))
;; ("parse-tree" "aa" (("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 3 4) "aaba" try-match)
;Value: #f                              ;

((p:repeat (p:string "a") 1 #f) "aaba" try-match)
;; ("parse-tree" "a" (("a")))
;; ("parse-tree" "aa" (("a") ("a")))
;; ;Value: #f

;; Testing greediness parameter

((p:repeat (p:string "a") 1 #f) "aaaba" try-match)
;; ("parse-tree" "a" (("a")))
;; ("parse-tree" "aa" (("a") ("a")))
;; ("parse-tree" "aaa" (("a") ("a") ("a")))
;; ;Value: #f

((p:repeat (p:string "a") 1 #f #t) "aaaba" try-match)
;; ("parse-tree" "aaa" (("a") ("a") ("a")))
;; ("parse-tree" "aa" (("a") ("a")))
;; ("parse-tree" "a" (("a")))
;; ;Value: #f

((p:repeat (p:string "kitty") 1 #f) "kittykittykittykittyhellokitty" try-match)
;; ("parse-tree" "kitty" (("kitty")))
;; ("parse-tree" "kittykitty" (("kitty") ("kitty")))
;; ("parse-tree" "kittykittykitty" (("kitty") ("kitty") ("kitty")))
;; ("parse-tree" "kittykittykittykitty"
;;               (("kitty") ("kitty") ("kitty") ("kitty")))
;; ;Value: #f

(let ((pattern
       (p:repeat (p:seq (p:choice (p:string "a") (p:string "b"))
                        (p:choice (p:string "c") (p:string "d")))
                 3 #f)))
  (pattern "acadbcbc" try-match))
;; ("parse-tree" "acadbc" ((("a") ("c")) (("a") ("d")) (("b") ("c"))))
;; ("parse-tree" "acadbcbc"
;;               ((("a") ("c")) (("a") ("d")) (("b") ("c")) (("b") ("c"))))
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
;; ("parse-tree" "acadbc" (test-rule (("a") ("c")) (("a") ("d")) (("b") ("c"))))
;; ("parse-tree"
;;  "acadbcbc"
;;  (test-rule (("a") ("c")) (("a") ("d")) (("b") ("c")) (("b") ("c"))))
;; ;Value: #f

((p:char-from "abc") "abc" try-match)
;; ("parse-tree" "a" ("a"))
;; ;Value: #f

((p:char-from "abc") "bbc" try-match)
;; ("parse-tree" "b" ("b"))
;; ;Value: #f

((p:char-from "abc") "xbc" try-match)
;; ;Value: #f

((p:char-not-from "abc") "abc" try-match)
;; ;Value: #f

((p:char-not-from "abc") "bbc" try-match)
;; ;Value: #f

((p:char-not-from "abc") "xbc" try-match)
;; ("parse-tree" "x" ("x"))
;; ;Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:* (p:seq (p:choice (p:string "a") (p:string "b"))
			   (p:choice (p:string "c") (p:string "d")))))))
  (pattern "adadbc" try-match))
;; ("parse-tree" "" (test-rule))
;; ("parse-tree" "ad" (test-rule (("a") ("d"))))
;; ("parse-tree" "adad" (test-rule (("a") ("d")) (("a") ("d"))))
;; ("parse-tree" "adadbc" (test-rule (("a") ("d")) (("a") ("d")) (("b") ("c"))))
;; ;Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:+ (p:seq (p:choice (p:string "a") (p:string "b"))
			   (p:choice (p:string "c") (p:string "d")))))))
  (pattern "adadbc" try-match))
;; ("parse-tree" "ad" (test-rule (("a") ("d"))))
;; ("parse-tree" "adad" (test-rule (("a") ("d")) (("a") ("d"))))
;; ("parse-tree" "adadbc" (test-rule (("a") ("d")) (("a") ("d")) (("b") ("c"))))
;; ;Value: #f

(let ((pattern
       (p:rule 'test-rule
               (p:? (p:seq (p:choice (p:string "a") (p:string "b"))
			   (p:choice (p:string "c") (p:string "d")))))))
  (pattern "adadbc" try-match))
;; ("parse-tree" "" (test-rule))
;; ("parse-tree" "ad" (test-rule (("a") ("d"))))
;; ;Value: #f

((p:require
  (lambda (parse-tree)
    (> (string-length (parse-tree-text parse-tree)) 3))
  (p:choice (p:string "h") (p:string "he") (p:string "hello")
	    (p:string "hello kitty")))
 "hello kitty :)"
 try-match)
;; ("parse-tree" "hello" ("hello"))
;; ("parse-tree" "hello kitty" ("hello kitty"))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #f (p:string "a") (p:string "b") (p:string "c")) "a    b        c" try-match)
;; ("parse-tree" "a    b        c" (("a") ("b") ("c")))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #t (p:string "a") (p:string "b") (p:string "c")) "a    b        c" try-match)
;; ("parse-tree"
;;  "a    b        c"
;;  (("a") ((" ") (" ") (" ") (" "))
;;         ("b")
;;         ((" ") (" ") (" ") (" ") (" ") (" ") (" ") (" "))
;;         ("c")))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #f (p:string "a") (p:string "b") (p:string "c")) "abc" try-match)
;; ("parse-tree" (("a") ("b") ("c")))
;; ;Value: #f

((p:separated-by (p:* (p:string " ")) #t (p:string "a") (p:string "b") (p:string "c")) "abc" try-match)
;; ("parse-tree" "abc" (("a") () ("b") () ("c")))
;; ;Value: #f

;; Testing repeat with empty string

((p:repeat (p:choice (p:string "") (p:string "x")) 3 5) "xxxx" try-match)
#|
("parse-tree" "" (("") ("") ("")))
("parse-tree" "x" (("x") ("") ("") ("")))
("parse-tree" "xx" (("x") ("x") ("") ("") ("")))
("parse-tree" "x" (("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("") ("")))
("parse-tree" "xx" (("x") ("") ("x") ("") ("")))
("parse-tree" "xx" (("x") ("x") ("") ("")))
("parse-tree" "xx" (("") ("x") ("x") ("") ("")))
("parse-tree" "xxx" (("x") ("x") ("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("")))
("parse-tree" "x" (("") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("") ("x") ("")))
("parse-tree" "xx" (("") ("x") ("") ("x") ("")))
("parse-tree" "xxx" (("x") ("x") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("x") ("")))
("parse-tree" "xx" (("") ("x") ("x") ("")))
("parse-tree" "xx" (("") ("") ("x") ("x") ("")))
("parse-tree" "xxx" (("x") ("") ("x") ("x") ("")))
("parse-tree" "xxx" (("x") ("x") ("x") ("")))
("parse-tree" "xxx" (("") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x") ("")))
("parse-tree" "x" (("") ("") ("x")))
("parse-tree" "x" (("") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("") ("") ("x")))
("parse-tree" "xxx" (("x") ("x") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("") ("x")))
("parse-tree" "xx" (("") ("") ("x") ("") ("x")))
("parse-tree" "xxx" (("x") ("") ("x") ("") ("x")))
("parse-tree" "xxx" (("x") ("x") ("") ("x")))
("parse-tree" "xxx" (("") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("x")))
("parse-tree" "xx" (("") ("") ("x") ("x")))
("parse-tree" "xx" (("") ("") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("") ("x") ("x")))
("parse-tree" "xxx" (("") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("x") ("x")))
("parse-tree" "xxx" (("") ("x") ("x") ("x")))
("parse-tree" "xxx" (("") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("x")))
;Value: #f
|#

((p:repeat (p:choice (p:string "") (p:string "x")) 3 #f) "xxxx" try-match)
#|
("parse-tree" "" (("") ("") ("")))
("parse-tree" "x" (("x") ("") ("") ("")))
("parse-tree" "xx" (("x") ("x") ("") ("") ("")))
("parse-tree" "xxx" (("x") ("x") ("x") ("") ("") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x") ("") ("") ("")))
("parse-tree" "x" (("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("") ("")))
("parse-tree" "xx" (("x") ("") ("x") ("") ("")))
("parse-tree" "xxx" (("x") ("x") ("") ("x") ("") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("x") ("") ("")))
("parse-tree" "xx" (("x") ("x") ("") ("")))
("parse-tree" "xx" (("") ("x") ("x") ("") ("")))
("parse-tree" "xxx" (("x") ("") ("x") ("x") ("") ("")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("x") ("") ("")))
("parse-tree" "xxx" (("x") ("x") ("x") ("") ("")))
("parse-tree" "xxx" (("") ("x") ("x") ("x") ("") ("")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("x") ("") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x") ("") ("")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("")))
("parse-tree" "x" (("") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("") ("") ("x") ("")))
("parse-tree" "xxx" (("x") ("x") ("") ("") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("") ("x") ("")))
("parse-tree" "xx" (("") ("x") ("") ("x") ("")))
("parse-tree" "xxx" (("x") ("") ("x") ("") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("") ("x") ("")))
("parse-tree" "xxx" (("x") ("x") ("") ("x") ("")))
("parse-tree" "xxx" (("") ("x") ("x") ("") ("x") ("")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("x") ("")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("x") ("")))
("parse-tree" "xx" (("") ("x") ("x") ("")))
("parse-tree" "xx" (("") ("") ("x") ("x") ("")))
("parse-tree" "xxx" (("x") ("") ("") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("") ("") ("x") ("x") ("")))
("parse-tree" "xxx" (("x") ("") ("x") ("x") ("")))
("parse-tree" "xxx" (("") ("x") ("") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("") ("x") ("") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("x") ("")))
("parse-tree" "xxxx" (("") ("x") ("x") ("") ("x") ("x") ("")))
("parse-tree" "xxx" (("x") ("x") ("x") ("")))
("parse-tree" "xxx" (("") ("x") ("x") ("x") ("")))
("parse-tree" "xxx" (("") ("") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("") ("") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("") ("x") ("") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("x") ("")))
("parse-tree" "xxxx" (("") ("") ("x") ("x") ("x") ("x") ("")))
("parse-tree" "x" (("") ("") ("x")))
("parse-tree" "x" (("") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("") ("") ("x")))
("parse-tree" "xxx" (("x") ("x") ("") ("") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("") ("") ("x")))
("parse-tree" "xxx" (("x") ("") ("x") ("") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("") ("") ("x")))
("parse-tree" "xxx" (("x") ("x") ("") ("") ("x")))
("parse-tree" "xxx" (("") ("x") ("x") ("") ("") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("") ("x")))
("parse-tree" "xx" (("") ("") ("x") ("") ("x")))
("parse-tree" "xxx" (("x") ("") ("") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("") ("x") ("") ("x")))
("parse-tree" "xxx" (("x") ("") ("x") ("") ("x")))
("parse-tree" "xxx" (("") ("x") ("") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("") ("x") ("") ("x")))
("parse-tree" "xxx" (("x") ("x") ("") ("x")))
("parse-tree" "xxx" (("") ("x") ("x") ("") ("x")))
("parse-tree" "xxx" (("") ("") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("") ("") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("") ("x") ("") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("") ("x")))
("parse-tree" "xxxx" (("") ("") ("x") ("x") ("x") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("x")))
("parse-tree" "xx" (("") ("") ("x") ("x")))
("parse-tree" "xx" (("") ("") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("") ("") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("") ("") ("x") ("x")))
("parse-tree" "xxx" (("") ("x") ("") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("") ("x") ("x")))
("parse-tree" "xxx" (("") ("x") ("") ("x") ("x")))
("parse-tree" "xxx" (("") ("") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("") ("x") ("x")))
("parse-tree" "xxxx" (("") ("") ("x") ("x") ("") ("x") ("x")))
("parse-tree" "xxx" (("x") ("x") ("x")))
("parse-tree" "xxx" (("") ("x") ("x") ("x")))
("parse-tree" "xxx" (("") ("") ("x") ("x") ("x")))
("parse-tree" "xxx" (("") ("") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("") ("x") ("") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("x") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("") ("x") ("x") ("x") ("x")))
("parse-tree" "xxxx" (("") ("") ("") ("x") ("x") ("x") ("x")))
;Value: #f
|#


((p:repeat (p:choice (p:string "") (p:string "x")) 0 #f) "xxxx" try-match)
#|
("parse-tree" "" ())
("parse-tree" "x" (("x")))
("parse-tree" "xx" (("x") ("x")))
("parse-tree" "xxx" (("x") ("x") ("x")))
("parse-tree" "xxxx" (("x") ("x") ("x") ("x")))
;Value: #f
|#

((p:seq (p:repeat (p:choice (p:string "") (p:string "x")) 0 #f) (p:string "y")) "z" try-match)
;; ;Value: #f

((p:seq (p:repeat (p:choice (p:string "") (p:string "x")) 0 #f) (p:string "z")) "z" try-match)
;; ("parse-tree" "z" (() ("z")))
;; ;Value: #f


((p:repeat (p:choice (p:string "") (p:string "x")) 3 5) "xx" try-match)
#|
("parse-tree" (("") ("") ("")))
("parse-tree" (("") ("") ("") ("x")))
("parse-tree" (("") ("") ("") ("x") ("x")))
("parse-tree" (("") ("") ("x")))
("parse-tree" (("") ("") ("x") ("")))
("parse-tree" (("") ("") ("x") ("") ("x")))
("parse-tree" (("") ("") ("x") ("x")))
("parse-tree" (("") ("") ("x") ("x") ("")))
("parse-tree" (("") ("x") ("")))
("parse-tree" (("") ("x") ("") ("")))
("parse-tree" (("") ("x") ("") ("") ("x")))
("parse-tree" (("") ("x") ("") ("x")))
("parse-tree" (("") ("x") ("") ("x") ("")))
("parse-tree" (("") ("x") ("x")))
("parse-tree" (("") ("x") ("x") ("")))
("parse-tree" (("") ("x") ("x") ("") ("")))
("parse-tree" (("x") ("") ("")))
("parse-tree" (("x") ("") ("") ("")))
("parse-tree" (("x") ("") ("") ("") ("x")))
("parse-tree" (("x") ("") ("") ("x")))
("parse-tree" (("x") ("") ("") ("x") ("")))
("parse-tree" (("x") ("") ("x")))
("parse-tree" (("x") ("") ("x") ("")))
("parse-tree" (("x") ("") ("x") ("") ("")))
("parse-tree" (("x") ("x") ("")))
("parse-tree" (("x") ("x") ("") ("")))
("parse-tree" (("x") ("x") ("") ("") ("")))
;Value: #f
|#

((p:repeat (p:choice (p:string "") (p:string "x")) 3 5) "x" try-match)
#|
("parse-tree" "" (("") ("") ("")))
("parse-tree" "x" (("x") ("") ("") ("")))
("parse-tree" "x" (("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("")))
("parse-tree" "x" (("") ("") ("x") ("")))
("parse-tree" "x" (("") ("") ("x")))
("parse-tree" "x" (("") ("") ("") ("x")))
;Value: #f
|#

((p:repeat (p:choice (p:string "") (p:string "x")) 3 5) "" try-match)
#|
("parse-tree" "" (("") ("") ("")))
;Value: #f
|#

((p:repeat (p:choice (p:string "") (p:string "x")) 3 #f) "xx" try-match)
#|
("parse-tree" "" (("") ("") ("")))
("parse-tree" "x" (("x") ("") ("") ("")))
("parse-tree" "xx" (("x") ("x") ("") ("") ("")))
("parse-tree" "x" (("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("") ("")))
("parse-tree" "xx" (("x") ("") ("x") ("") ("")))
("parse-tree" "xx" (("x") ("x") ("") ("")))
("parse-tree" "xx" (("") ("x") ("x") ("") ("")))
("parse-tree" "x" (("") ("x") ("")))
("parse-tree" "x" (("") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("") ("x") ("")))
("parse-tree" "xx" (("") ("x") ("") ("x") ("")))
("parse-tree" "xx" (("x") ("x") ("")))
("parse-tree" "xx" (("") ("x") ("x") ("")))
("parse-tree" "xx" (("") ("") ("x") ("x") ("")))
("parse-tree" "x" (("") ("") ("x")))
("parse-tree" "x" (("") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("") ("") ("x")))
("parse-tree" "xx" (("x") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("") ("x")))
("parse-tree" "xx" (("") ("") ("x") ("") ("x")))
("parse-tree" "xx" (("") ("x") ("x")))
("parse-tree" "xx" (("") ("") ("x") ("x")))
("parse-tree" "xx" (("") ("") ("") ("x") ("x")))
;Value: #f
|#
