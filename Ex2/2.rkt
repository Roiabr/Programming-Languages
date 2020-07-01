#lang pl

;; --------------- Question 1 ------------------

#|
 This question was ok because it was like AE that i saw in class
 I had some difficult in parse-sexpr-RegL function in the base case in
 the recursion
|#

;;Q1.1
;; Defining two new types
(define-type BIT = (U 0 1))
(define-type Bit-List = (Listof BIT))

;; The actual interpreter
#| BNF for the ROL language:
<ROL> ::= {reg-len = <num> <RegE>}

<RegE> ::= <Bits>
         | {and <RegE> <RegE>}
         | {or <RegE> <RegE>}
         | {shl <RegE>}
        

<Bits> ::= <bit>
         | <bit> <Bits>

<bit> ::= 1 | 0
|#

#|
  examples: 1) <ROL> -> {reg-len = <num> <RegE>} - > {reg-len = 2 <RegE>} -> {reg-len = 2 <Bits>} - > {reg-len = 2 <bit> <Bits>} - > {reg-len = 2 <bit> <bit>} - > {reg-len = 2 1 0}
            2) <ROL> -> {reg-len = <num> <RegE>} - > {reg-len = <num> {shl <RegE>}} - > {reg-len = <num> {shl <Bits>}} - > {reg-len = 1 {shl <Bits>}} - > {reg-len = 1 {shl <bit>}} - > {reg-len = 1 {shl 1}}
            3) <ROL> -> {reg-len = <num> <RegE>} -> {reg-len = 3 <RegE>} -> {reg-len = 3 {and <RegE> <RegE>}} -> {reg-len = 3 {and <Bits> <Bits>}} -> {reg-len = 3 {and <bit> <Bits> <bit> <Bits>}} -> {reg-len = 3 {and <bit> <bit> <Bits> <bit> <bit> <Bits>}} 
                     -> {reg-len = 3 {and <bit> <bit> <bit> <bit> <bit> <bit>}} -> {reg-len = 3 {and  1 1 0  1 1 1}}
|#

;;Q1.2
;; RegE abstract syntax trees
(define-type RegE
  [Reg Bit-List]
  [And RegE RegE]
  [Or RegE RegE]
  [Shl RegE]
)


;; Next is a technical function that converts (casts)
;; (any) list into a bit-list. We use it in parse-sexpr.
(: list->bit-list : (Listof Any) -> Bit-List)
;; to cast a list of bits as a bit-lists
(define (list->bit-list lst)
  (cond [(null? lst) null]
        [(eq? (first lst) 1)(cons 1 (list->bit-list (rest lst)))]
        [else (cons 0 (list->bit-list (rest lst)))]))

(: parse-sexpr : Sexpr -> RegE)
;; to convert the main s-expression into ROL
(define (parse-sexpr sexpr)
  (match sexpr
    [(list 'reg-len `= (number: n) args)
     (if(> n 0)
        (parse-sexpr-RegL args n)
        (error 'parse-sexpr "Register length must be bigger then 0 ~s" sexpr))] ;; remember to make sure specified register length is at least 1
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))



(: parse-sexpr-RegL : Sexpr Number -> RegE)
;; inside function to convert s-expressions into RegE
(define (parse-sexpr-RegL sexpr len)
(match sexpr
  [(list (and a (or 1 0)) ... ) (if(eq? len (length a))
                                   (Reg(list->bit-list a))
                                   (error 'parse-sexpr "wrong number of bits in ~s" a))]
  [(list 'and list1 list2)
     (And(parse-sexpr-RegL list1  len) (parse-sexpr-RegL list2  len))]
  [(list 'or list1 list2)
     (Or(parse-sexpr-RegL list1  len)(parse-sexpr-RegL list2  len))]
  [(list 'shl list)
     (Shl(parse-sexpr-RegL list  len))]
  [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))


(: parse : String -> RegE)
;; parses a string containing a RegE expression to a RegE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))



;; tests
(test (parse "{ reg-len = 4 {1 0 0 0}}") => (Reg '(1 0 0 0)))
(test (parse "{ reg-len = 4 {shl {1 0 0 0}}}") => (Shl (Reg '(1 0 0 0))))
(test (parse "{ reg-len = 4 {and {shl {1 0 1 0}} {shl {1 0 1 0}}}}") => (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 1 0)))))
(test (parse "{ reg-len = 4 { or {and {shl {1 0 1 0}} {shl {1 0 0 1}}} {1 0 1 0}}}") => (Or (And (Shl (Reg '(1 0 1 0))) (Shl (Reg '(1 0 0 1)))) (Reg '(1 0 1 0))))
(test (parse "{ reg-len = 2 { or {and {shl {1 0}} {1 0}} {1 0}}}") => (Or (And (Shl (Reg '(1 0))) (Reg '(1 0))) (Reg '(1 0))))
(test (parse "{ reg-len = 4 {or {1 1 1 1} {0 1 1}}}") =error> "wrong number of bits in")
(test (parse "{ reg-len = 0 {}}") =error> "Register length must be bigger then 0")
(test (parse "{{1 0 0 0}}") =error> "bad syntax in ((1 0 0 0))")
(test (parse "{ reg-len = 3 {+ {1 1 1} {0 1 1}}}")  =error> " bad syntax in (+ (1 1 1) (0 1 1))")

;; --------------- Question 2 ------------------
#|
  In this Question i had a lot of problems and was very difficult for me.it took me most of the time to solve.
|#
;;Q2_1
#|
The problem: we can get two diffrent bnf that give us a diffrent answer:
1. first we "set 1", second we "set 2", and now in the memory cell we have "2", now we sub 1+2 and multiplies this with 2(by get operation- get 2 from the cell)
   we get (*(+ 1 2) 2)=6
2. first we "set 2", second we "set 1", and now in the memory cell we have "1", now we sub 1+2 and multiplies this with 1(by get operation- get 2 from the cell)
   we get (*(+ 1 2) 1)=3
To solve This problem we can set the most right set in the end  and that will assure us one answer.
|#
;;Q2_2
#|
  In this question we need to write a new MAE grammar that derives a programs which a non-empty sequence of sub-computations
  each one gets stored in the memory and is available for the following sub-computation, except for the last one.
  we need to add a 3 different MAE expressions.
  

<MAE> ::= {seq <AE>}
            | {seq <SET>}

<SET> ::= {set <NUM-GET>}
            | {set <NUM-GET>} <SET>
            | <NUM-GET>

<AE> :: = <num>
                 | {+ <AE> <AE>}
                 | {- <AE> <AE>} 
                 | {* <AE> <AE>} 
                 | {/ <AE> <AE>} 
                 | <AE>
 
<NUM-GET> ::=  <num> 
                 | get
                 | {+ <NUM-GET> <NUM-GET>} 
                 | {- <NUM-GET> <NUM-GET>} 
                 | {* <NUM-GET> <NUM-GET>} 
                 | {/ <NUM-GET> <NUM-GET>} 



1) input: 311505481
   <MAE> -> {seq <GET-AE>} -> {seq <GET-AE>} ->{seq {- {<GET-AE>} {<GET-AE>}} } ->  {seq {- 311 505}} 

2) input: 311505481
   <MAE> -> {seq <SET>} -> {seq  {set {<NUM-GET>}} <SET>} -> {seq  {set {<NUM-GET>}} {set {<NUM-GET>}} <SET>} -> {seq  {set {<NUM-GET>}} {set {<NUM-GET>}} <NUM-GET>} ->
   {seq  {set {+ <NUM-GET> <NUM-GET>} } {set {* <NUM-GET> <NUM-GET>} } {/ <NUM-GET> <NUM-GET>} } -> {seq  {set {+ <NUM-GET> <NUM-GET>} } {set {* get get} } {/ get <NUM-GET>} } ->
   {seq  {set {+ 5054 311} } {set {* get get} } {/ get 1} }

3) input: 3115054881

<MAE> -> {seq <SET>} -> {seq {set {<NUM-GET>}} <SET>} -> {seq {set {- <NUM-GET> <NUM-GET>} <NUM-GET>} -> {seq {set {- <NUM-GET> <NUM-GET>}} get}
(1)-> {seq {set {- <num> <num>}} get} -> {seq {set {- 311505 481}} get}

|#


;; --------------- Question 3 ------------------
;;Q3
#|
   In this question we need to write function which takes a list of numbers as input, and produces a number which is the sum
   of the squares of all of the numbers in the list.
|#


(: multi : Number -> Number)
;; function that help us to multiply each element.
(define (multi n)
  (* n n))

(: sum-of-squares : (Listof Number) -> Number)
(define (sum-of-squares len)
  (let ([result (map multi len)]) 
    (foldl + 0 result)     ;; use foldl to calculate the result 
  )
)


;;tests:
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(2)) => 4)
(test (sum-of-squares '(-2)) => 4)
(test (sum-of-squares '(1 1 1)) => 3)
(test (sum-of-squares '(0 0 0)) => 0)
(test (sum-of-squares '(1 2 3)) => 14)
(test (sum-of-squares '(2 2 2)) => 12)
(test (sum-of-squares '(-1 -2 -3)) => 14)
(test (sum-of-squares '(-4 4 -2)) => 36)
(test (sum-of-squares '(-1 -2 -3 -4)) => 30)
(test (sum-of-squares '(-4 4 -2 2)) => 40)



;; --------------- Question 4 ------------------
;; BINTREE abstract syntax trees
(define-type BINTREE
   [Node  BINTREE  BINTREE]
   [Leaf Number])


;; This function get a function that operate on each item in the BINTREE and 
;; returns The BINTREE 
(: tree-map : (Number -> Number) BINTREE -> BINTREE)
(define (tree-map Fun btree)
  (cases btree
    [(Leaf num) (Leaf (Fun num))]
    [(Node node1 node2) (Node (tree-map Fun node1) (tree-map Fun node2))]
  )
)


(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
;; The Function get A combiner function, operating function over items(like tree-map),
;; and a BINTREE and returns Combined BINTREE 
(define (tree-fold f fun tree)
  (cases tree
    [(Leaf num) (fun num)]
    [(Node Lnode Rnode) (f (tree-fold f fun Lnode) (tree-fold f fun Rnode))]
  )
)

(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in
;; left-to-right order 
(define (tree-flatten tree)
  (tree-fold (inst append Number)
             (inst list Number) tree))


(: switch-nodes : BINTREE BINTREE -> BINTREE)
;; This is a help function for tree-reverse that get 2 Node
;; and switch between left and right node
(define (switch-nodes Lnode Rnode)
    (Node Rnode Lnode))

(: tree-reverse : BINTREE -> BINTREE)
;; Function that consumes a BINTREE and returns a BINTREE that is its mirror image.
(define (tree-reverse Btree)
  (tree-fold switch-nodes Leaf Btree))




;;Tests
(test (tree-map + (Node (Leaf 1) (Node (Leaf 2)                                          
(Leaf 3))))=> (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
(test (tree-map - (Node (Leaf 1) (Node (Leaf 2)                                          
(Leaf 3))))=> (Node (Leaf -1) (Node (Leaf -2) (Leaf -3))))
(test (equal? (tree-flatten (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))
 (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))))=> #t )
(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))=> '(1 2 3) )
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2)                                          
(Leaf 3))))=> (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))=> '(1 2 3) )
(test (tree-flatten (Node (Node (Node (Leaf 3) (Leaf 4))
(Node (Leaf 5) (Leaf 6))) (Node (Leaf 7) (Leaf 8))))=> '(3 4 5 6 7 8 ) )
(test(equal? (reverse (tree-flatten (Node (Leaf 1) (Node (Leaf 2)(Leaf 3))))) (tree-flatten (tree-reverse (Node (Leaf 1) (Node (Leaf 2)(Leaf 3)))))) => #t)
(test (tree-reverse (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))  => (Node (Node (Leaf 3) (Leaf 2)) (Leaf 1)))
