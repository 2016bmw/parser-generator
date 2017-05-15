(define-record-type <parse-tree>
  (%make-parse-tree data text children)
  parse-tree?
  (data parse-tree-data)
  (text parse-tree-text)
  (children parse-tree-children))

(define make-parse-tree %make-parse-tree)

(define (build-node-from-children-stack children)
  (define (string-join string-list)
    (apply string-append string-list))
  (let lp ((remaining-children children) (data '()) (text '()))
    (if (null? remaining-children)
	(make-parse-tree data (string-join text) (reverse children))
	(lp (cdr remaining-children)
	    (cons (parse-tree-data (car remaining-children)) data)
	    (cons (parse-tree-text (car remaining-children)) text)))))

