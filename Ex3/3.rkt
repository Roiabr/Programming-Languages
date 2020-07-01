#lang pl 

#| BNF for the Muwae language:
       <Muwae> ::= <num>
               | { + <Muwae> <Muwae> }
               | { - <Muwae> <Muwae> }
               | { * <Muwae> <Muwae> }
               | { / <Muwae> <Muwae> }
               | { sqrt <Muwae> }
               | { with { <id> <Muwae> } <Muwae> }
               | <id>
  |#

;; Muwae abstract syntax trees
(define-type Muwae
  [Num  (Listof Number)]
  [Add  Muwae Muwae]
  [Sub  Muwae Muwae]
  [Mul  Muwae Muwae]
  [Div  Muwae Muwae]
  [Sqrt Muwae]
  [Id   Symbol]
  [With Symbol Muwae Muwae])

(: parse-sexpr : Sexpr -> Muwae)
;; to convert s-expressions into Muwaes
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num(list n))]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt expr) (Sqrt (parse-sexpr expr))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> Muwae)
;; parses a string containing a Muwae expression to a Muwae AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
     (`N' is a <num>, `E1', `E2' are <Muwae>s, `x' is some <id>, `y' is a
     *different* <id>)
        N[v/x]                = N
        {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
        {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
        {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
        {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
        y[v/x]                = y
        x[v/x]                = v
        {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
        {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
  |#

(: subst : Muwae Symbol Muwae -> Muwae)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Sqrt expr) (Sqrt (subst expr from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]))

#| Formal specs for `eval':
       eval(N)         = N
       eval({+ E1 E2}) = eval(E1) + eval(E2)
       eval({- E1 E2}) = eval(E1) - eval(E2)
       eval({* E1 E2}) = eval(E1) * eval(E2)
       eval({/ E1 E2}) = eval(E1) / eval(E2)
       eval(id)        = error!
       eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
  |#


(: eval : Muwae -> (Listof Number))
;; evaluates Muwae expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n)  n]
    [(Add l r) ( bin-op  +  (eval l)  (eval r))]
    [(Sub l r) ( bin-op  -  (eval l)  (eval r))]
    [(Mul l r) ( bin-op  *  (eval l)  (eval r))]
    [(Div l r) ( bin-op  /  (eval l)  (eval r))]
    [(Sqrt expr) (sqrt+ (eval expr))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    ))

(: sqrt+ : (Listof Number) -> (Listof Number))
;; In this function we get a list of number and do sqrt of every number of the list(in recursive way)
;; the function check if the number is negative and if not - return the sqrt of the number in both
;; positve and negative answer
(define (sqrt+ num)
  (cond [(null? num) num]
        [(< (first num) 0) (error 'sqrt "please use only a nonnegative input")];; cant do sqrt on negative number
        [else (let ([exp (sqrt (first num))])(append (list exp (* -1 exp)) (sqrt+ (rest num))))]));;run in recusive way on every number in the lust

(: bin-op : (Number Number -> Number) (Listof Number) (Listof Number) -> (Listof Number))
;; applies a binary numeric function on all combinations of numbers from
;; the two input lists, and return the list of all of the results
(define (bin-op op ls rs)
  (: helper : Number (Listof Number) -> (Listof Number))
  (define (helper l rs);; l-is the first number of the left side list
    (: f : Number -> Number)
    (define (f exp)
      (op l exp))
    (map f rs));;Every iteration we send a number from the right side list
  (if (null? ls) null
      (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

(: run : String -> (Listof Number))
;; evaluate a MUWAE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "5") => '(5))
(test (run "{+ 5 5}") => '(10))
(test (run "{with {x {+ 2 5}} {+ x x}}") => '(14))
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))
(test (run "{with {x 1} y}") =error> "free identifier")
(test (run "{sqrt 8}") => '(2.8284271247461903 -2.8284271247461903))
(test (run "{sqrt 4}") => '(2 -2))
(test (run "{sqrt 0}") => '(0 0))
(test (run "{sqrt -1}") =error>  "please use only a nonnegative input")
(test (run "{sqrt {with {x {/ 25 5}} {* x x}}}") => '(5 -5))
(test (run "{sqrt {with {x {+ 18 3}} {with {y {- x 3}} {+ y y}}}}") => '(6 -6))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {/ x {with {y 3} x}}}") => '(1))
(test (run "{with {x 5}}") =error> "parse-sexpr: bad `with' syntax in (with (x 5))")
(test (run "{with {x 5} {sqrt x {with {y 3} x}}}") =error> "parse-sexpr: bad syntax in (sqrt x (with (y 3) x)" )
(test (run "{with {x 5} {sqrt x}}") => '(2.23606797749979 -2.23606797749979))
(test (run "{+ {sqrt 1} 3}") => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}")
=> '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}")
=> '(5 -5 4 -4))



#|                              part B            |#


;; wae abstract syntax trees
#|
<WAE> ::= <num> 
   | {+ <WAE> <WAE>}
   | {-  <WAE> <WAE>}
   | {* <WAE> <WAE>}
   | {/ <WAE> <WAE>}
   | {with {<id> <WAE>} <WAE>}
   | <id>

|#



(define-type WAE
  [NumW Number]
  [AddW WAE WAE]
  [SubW WAE WAE]
  [MulW WAE WAE]
  [DivW WAE WAE]
  [IdW Symbol]
  [WithW Symbol WAE WAE])


;; to convert s-expressions into WAE
(: parse-sexprW : Sexpr -> WAE) 
(define (parse-sexprW sexpr)
  (match sexpr
    [(number: n) (NumW n)]
    [(symbol: name) (IdW name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (WithW name (parse-sexprW named) (parse-sexprW body))]
       [else (error 'parse-sexprW "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (AddW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '- lhs rhs) (SubW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '* lhs rhs) (MulW (parse-sexprW lhs) (parse-sexprW rhs))]
    [(list '/ lhs rhs) (DivW (parse-sexprW lhs) (parse-sexprW rhs))]
    [else (error 'parse-sexprW "bad syntax in ~s" sexpr)]))

;; to convert String to s-expressions and get WAE
(: parseW : String -> WAE)
(define (parseW str)
  (parse-sexprW (string->sexpr str)))



#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <WAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#



(: substW : WAE Symbol WAE -> WAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (substW expr from to)
  (cases expr
    [(NumW n) expr]
    [(AddW l r) (AddW (substW l from to) (substW r from to))]
    [(SubW l r) (SubW (substW l from to) (substW r from to))]
    [(MulW l r) (MulW (substW l from to) (substW r from to))]
    [(DivW l r) (DivW (substW l from to) (substW r from to))]
    [(IdW name) (if (eq? name from) to expr)]
    [(WithW bound-id named-expr bound-body)
     (WithW bound-id
           (substW named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (substW bound-body from to)))]))


;; In this function we get a list of number and do sqrt of every number of the list(in recursive way)
;; the function check if the number is negative and if not - return the sqrt of the number in both
;; positve and negative answer

(: freeInstanceList : WAE -> (Listof Symbol))
;;the function get WAE and Checking for syntactic problem
;;every varible checking if it free instance
;;return list of free instance
(define (freeInstanceList expr)
  (cases expr
    [(NumW n) null]
    [(AddW l r) (append  (freeInstanceList l)  (freeInstanceList r))]
    [(SubW l r) (append  (freeInstanceList l)  (freeInstanceList r))]
    [(MulW l r) (append  (freeInstanceList l)  (freeInstanceList r))]
    [(DivW l r) (append  (freeInstanceList l)  (freeInstanceList r))]
     [(WithW bound-id named-expr bound-body)
     (append  (freeInstanceList named-expr)
            (freeInstanceList (substW bound-body
                                bound-id
                (NumW 5))))]
    [(IdW name) (list name)]))



;;test part B
(test (freeInstanceList (parseW "w")) => '(w))
(test (freeInstanceList (parseW "{with {xxx 2} {with {yyy 3} {+ {- xx y} z}}}")) => '(xx y z))
(test (freeInstanceList (parseW "{with {gal 26} {with {hadida 3} {+ {- gal y} z}}}")) => '( y z))
(test (freeInstanceList (WithW 'x (NumW 2) (AddW (IdW 'x) (NumW 3)))) => '())
(test (freeInstanceList (parseW "{+ z {+ x z}}")) => '(z x z))
(test (freeInstanceList (parseW "{with {x 5} {with {y {/ z 3}} {+ y y}}}")) => '(z))
(test (freeInstanceList (parseW "{with {t z} {+ t t}}")) => '(z))
(test (freeInstanceList (parseW "{with {x 3} {+ x {with {x {+ x x}}}}}"))=error> " bad `with' syntax in (with (x (+ x x)))")
                                


