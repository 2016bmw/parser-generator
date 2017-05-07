(define (p:string string)
  (let ((str-len (string-length string)))
    (define (string-match data success)
      ;; (pp (list "string-match" string data))
      (and (>= (string-length data) str-len)
           (string=? string (substring data 0 str-len))
           (success (list string) str-len)))
    string-match))

;;; Match single character that satisfies the predicate. Can be used
;;; to create matcher for number, whitespace, alphanumeric, etc.
(define (p:char-predicate char-predicate)
  (define (char-predicate-matcher data success)
      (and (> (string-length data) 0)
	   (char-predicate (car (string->list data)))
	   (success (list (substring data 0 1)) 1)))
  char-predicate-matcher)

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

;;; P:REPEAT matches between MIN and MAX occurrences of the MATCHER, unbounded
;;; if MAX is #f. If GREEDY is true, try to match as many occurrences as
;;; possible. Otherwise, if GREEDY is #f or not provided, match as few as
;;; occurrences as possible.
;;;
;;; If the empty string is a possible match for the base MATCHER, the empty string can 
;;; occur at most MIN times in the parse tree for the repeat match. While this specification 
;;; is only truly necessary for unbounded repeat intervals, it is applied to bounded intervals as 
;;; well for the sake of consistency. With this constraint in place, unbounded intervals are still 
;;; able to match the empty string, but cannot produce infinite parse trees and prevent matching 
;;; from completing.
(define (p:repeat matcher min max #!optional greedy)
  (let ((greedy (if (default-object? greedy) #f greedy)))
    (define (repeat-match data success)
      (let lp ((i 0)
               (cur-data data)
               (cur-parse-tree (list))
	       (num-empty-matches 0))
        (define (try-base)
          (matcher cur-data
                   (lambda (parse-tree num-consumed)
                     (let ((empty-match? (zero? num-consumed)))
		       (define (loop-again)
			 (lp (+ i 1)
			     (substring cur-data num-consumed (string-length cur-data))
			     (append cur-parse-tree (list parse-tree))
			     (if empty-match?
				 (+ num-empty-matches 1)
				 num-empty-matches)))

		       (if (and empty-match? (>= num-empty-matches min))
			   #f
			   (loop-again))))))
        (define (try-finish)
          (and (>= i min)
               (success cur-parse-tree (- (string-length data)
                                          (string-length cur-data)))))
        (if (and max (> i max))
            #f
            (if greedy
                (or (try-base) (try-finish))
                (or (try-finish) (try-base))
                ))))
    repeat-match))

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


(define (p:separated-by separator include-in-parse-tree? . matchers)
  (define (separate-list-with lst separator)
    (if (or (null? lst) (null? (cdr lst)))
	lst
	(cons (car lst) (cons separator (separate-list-with (cdr lst) separator)))))
  (define (skip-every-other-element lst)
    (if (or (null? lst) (null? (cdr lst)))
	lst
	(cons (car lst) (skip-every-other-element (cddr lst)))))	   
  (define (separated-matcher data success)
    ((apply p:seq (separate-list-with matchers separator))
     data
     (if include-in-parse-tree?
	 success
	 (lambda (parse-tree num-consumed)
	   (success (skip-every-other-element parse-tree) num-consumed)))))
  separated-matcher)

(define (p:transform f matcher)
  (define (transform-matcher data success)
    (matcher data
             (lambda (parse-tree num-consumed)
               (success (f parse-tree)
                        num-consumed))))
    transform-matcher)

(define (p:require f matcher)
  (define (require-matcher data success)
    (matcher data
	     (lambda (parse-tree num-consumed)
	       (and (f parse-tree)
		    (success parse-tree num-consumed)))))
  require-matcher)

;;; Convenience combinators
(define (p:char-from string)
  (let ((choices (string->list string)))
    (p:char-predicate (lambda (char) (memq char choices)))))

(define (p:char-not-from string)
  (let ((choices (string->list string)))
    (p:char-predicate (lambda (char) (not (memq char choices))))))

(define (p:+ matcher)
  (p:repeat matcher 1 #f))

(define (p:? matcher)
  (p:repeat matcher 0 1))

(define (p:* matcher)
  (p:repeat matcher 0 #f))

;;; Parse tree functions

;; TODO change this when we change parse-tree format
(define (parse-tree-data parse-tree) parse-tree) 
