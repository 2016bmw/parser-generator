# parser-generator
6.905/945 final project

A parser is something that takes an input string and tries to match it against some grammar in order to generate a syntax tree. For example, a parser for an arithmetic expression may perform like the following:

```
(parse-expression "1 + 2 * (3 + x) + ((4)) * 5")
(sum (product (number . 1))
     (product (number . 2)
              (sum (product (number . 3)) (product (var . x))))
     (product (sum (product (sum (product (number . 4)))))
              (number . 5)))
```

A parser generator, then, is a tool that takes user-defined grammar and generates a parser. Parser generators are convenient because they remove the need to hand-write a parser for every specific use case one might have. Instead, a programmer can simply write up the grammar in an easily expressible format that describes the domain of interest, and the parser generator transforms the grammar into an associated parser. 

This project is a parser generator that allows users to define grammar using scheme expressions. For example, a grammar to match integers can be defined as follows:

```
;;; number ::= [0-9]+
(define number-rule
  (p:rule 'number (p:+ (p:char-predicate char-numeric?))))
```

Our parser generator also allows user to transform the parse tree as it is being generated. When seeing a string that looks like a number, the `number-rule` presented above generates a tree where with each node being a digit string. For example, "123" would generate a tree with three leaf nodes:

```
    (+)
   / | \   
"1" "2" "3"
```

This is not very convenient to use. The user can instead write a grammar that transform it into a single integer-looking node:

```
;;; number ::= [0-9]+
(define number-rule
  (p:rule 'number
          (p:transform
           (lambda (parse-tree)
             (let ((string (apply string-append
                                  (apply append parse-tree))))
               (string->number string)))
           (p:+ (p:char-predicate char-numeric?)))))
```

Rules for variables, products, and sums can be written using our various primitives. In `expression-examples.scm` we show how to fully define a parser for this grammar:

```
#|
decimal  ::= [0-9]+ '.'? [0-9]*
           | '.' [0-9]+
scinot   ::= decimal ('E' | 'e') '-'? [0-9]+
rational ::= [0-9]+ '/' [0-9]+

number   ::= '-'? (decimal | scinot | rational)
var      ::= [a-zA-Z]+
atom     ::= number | var | '(' sum ')'
product  ::= atom ('*' atom)*
sum      ::= product ('*' product)*
|#
```
