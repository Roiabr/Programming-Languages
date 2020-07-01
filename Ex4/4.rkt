;; The Flang interpreter
#|                  Gal hadida & Roi abramovitch                  |#
#lang pl
#|
we split the work between us ,roi fill the all the missing lines in parse-sexpr ,subst
gal focus on upgrade eval and logic-op
the hard part was the eval.
|#


#|
  The grammar:
    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }
              | { < <FLANG> <FLANG> }
              | { > <FLANG> <FLANG> }
              | { = <FLANG> <FLANG> }
              | { if <FLANG> <FLANG> }
              | { not <FLANG> > }
             

  Evaluation rules:

    subst:
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]} ; if y =/= x
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {call E1 E2}[v/x]     = {call E1[v/x] E2[v/x]}
      {fun {y} E}[v/x]      = {fun {y} E[v/x]}           ; if y =/= x
      {fun {x} E}[v/x]      = {fun {x} E}
      B[v/x] = B ;; B is Boolean
      {= E1 E2}[v/x] = {= E1[v/x] E2[v/x]}
      {> E1 E2}[v/x] = {> E1[v/x] E2[v/x]}
      {< E1 E2}[v/x] = {< E1[v/x] E2[v/x]}
      { not E}[v/x] = {not E[v/x]}
      {if Econd {then-do Edo} {else-do Eelse}}[v/x]
                               = {if Econd[v/x] {then-do Edo[v/x]} {else-doEelse[v/x]}}

    eval:
      eval(N)            = N
      eval({+ E1 E2})    = eval(E1) + eval(E2)  \ if both E1 and E2
      eval({- E1 E2})    = eval(E1) - eval(E2)   \ evaluate to numbers
      eval({* E1 E2})    = eval(E1) * eval(E2)   / otherwise error!
      eval({/ E1 E2})    = eval(E1) / eval(E2)  /
      eval(id)           = error!
      eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
      eval(FUN)          = FUN ; assuming FUN is a function expression
      eval({call E1 E2}) = eval(Ef[eval(E2)/x]) if eval(E1)={fun {x}Ef}
                         = error!               otherwise

  |#

(define-type FLANG
  [Num  Number]
  [Bool Boolean]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG]
  [Bigger FLANG FLANG]
  [Smaller FLANG FLANG]
  [Equal  FLANG FLANG]
  [Not FLANG]
  [If FLANG FLANG FLANG])

 

 
;;added If and logic-op (<,>,=,not)
(: parse-sexpr : Sexpr -> FLANG)
;; to convert s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
    ['True (Bool true)]
    ['False (Bool false)]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '= lhs rhs) (Equal (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '< lhs rhs) (Smaller (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '> lhs rhs) (Bigger (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'not exp) (Not (parse-sexpr exp))]
    [(cons 'if body)
     (match sexpr
       [(list 'if a (list 'then-do b) (list 'else-do c))
        (If (parse-sexpr a)(parse-sexpr b) (parse-sexpr c))]
       [else (error 'parse-sexpr "bad `if' syntax in ~s" sexpr)])]
    [(list 'call fun arg) (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))


;;added subst case for If and logic-op
;;in If susbst the three cases Econd,then-do,Edo
(: subst : FLANG Symbol FLANG -> FLANG)
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
    [(Id name) (if (eq? name from) to expr)]
    [(Bool b) expr]
    [(Equal l r) (Equal (subst l from to) (subst r from to))]
    [(Bigger l r) (Bigger (subst l from to) (subst r from to))]
    [(Smaller l r) (Smaller (subst l from to) (subst r from to))]
    [(Not l) (Not (subst l from to))]
    [(If list1 list2 list3) (If (subst list1 from to) (subst list2 from to) (subst list3 from to))]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
               bound-body
               (subst bound-body from to)))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
         expr
         (Fun bound-id (subst bound-body from to)))]))

(: Num->number : FLANG -> Number)
(define (Num->number e)
  (cases e
    [(Num n) n]
    [else (error 'Num->number "expected a number, got: ~s" e)]))

(: arith-op : (Number Number -> Number) FLANG FLANG -> FLANG)
;; gets a Racket numeric binary operator, and uses it within a FLANG
;; `Num' wrapper
(define (arith-op op expr1 expr2)
  (Num (op (Num->number expr1) (Num->number expr2))))


(: logic-op : (Number Number -> Boolean) FLANG FLANG -> FLANG)
;; gets a Racket Boolean binary operator (on numbers), and applies it
;; to two `Num' wrapped FLANGs
(define (logic-op op expr1 expr2)
  (Bool (op (Num->number expr1) (Num->number expr2))))

(: flang->bool : FLANG -> Boolean)
;; gets a Flang E (of any kind) and returns a its appropiate
;; Boolean value -- which is true if and only if E does not
;; represent false
;; Remark: the `flang->bool` function will also be top-level
;; since it's used in more than one place.
(define (flang->bool e)
  (cases e
    [(Bool va)  va]
    [(Num val) (if (< -1 val) true false)]
    [else (error 'flang->bool "free identifier: ~s" e)]))


;;in If cases we check which type of condation boolean value
;;and act properly
(: eval : FLANG -> FLANG)
;; evaluates FLANG expressions by reducing them to *expressions*
(define (eval expr)
  (cases expr
    [(Num n) expr]
    [(Bool b) expr]
    [(Add l r) (arith-op + (eval l) (eval r))]
    [(Sub l r) (arith-op - (eval l) (eval r))]
    [(Mul l r) (arith-op * (eval l) (eval r))]
    [(Div l r) (arith-op / (eval l) (eval r))]         
    [(If list1 list2 list3)
     (let([Econd (flang->bool (eval list1))])
       (if (eq? Econd true)
           (eval list2)
           (eval list3)))]    
    [(Equal l r)   (logic-op = (eval l) (eval r))]
    [(Bigger l r)  (logic-op > (eval l) (eval r))]
    [(Smaller l r) (logic-op < (eval l) (eval r))]
    [(Not l) (Bool(not (flang->bool (eval l))))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (eval named-expr)))]
    [(Id name) (error 'eval "free identifier: ~s" name)]
    [(Fun bound-id bound-body) expr]
    [(Call fun-expr arg-expr)
     (let([fval (eval fun-expr)])
       (cases fval
         [(Fun bound-id bound-body)
          (eval (subst bound-body
                       bound-id
                       (eval arg-expr)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                      fval)]))]))
 

;;we add 2 more cases ,one for boolean type and one for Fun constractor
(: run : String -> (U Number Boolean FLANG))
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str))])
    (cases result
      [(Num n) n]
      [(Bool val)  val]
      [(Fun  Symbol FLANG) result]  
      [else result])))





;; tests
(test (run "True") => true)
(test (run "{not True}") => false)
(test (run "{> 3 44}") => false)
(test (run "{if {- 3 3} {then-do 4} {else-do 5}}") => 4)
(test (run "{if {- 3 4} {then-do 4} {else-do 17}}") => 17)
(test (run "{with {x 8} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 1/4)
(test (run "{with {x 0} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 0)
(test (run "{if {> 2 1} {then-do True} {else-do {+ 2 2}}}") => true)
(test (run "{< True 5}") =error> "Num->number: expected a number, got: #(struct:Bool #t)")

(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {call add3 1}}")
      => 4)
  
(test (run "{fun {x}}") =error> "bad `fun' syntax in" )

(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                    {call add1
                     {call add3 x}}}}}")
      => 7)

(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 {fun {x} {+ x 1}}}
                  {with {x 3}
                     {call add3 x}}}}")
      => 6)
(test (run "{with {add3 {fun {x} {+ x 3}}}
                {with {add1 True}
                  {with {x 3}
                    {call add1 {call add3 x}}}}}")
      =error> "eval: `call' expects a function, got:")

(test (run "{with {x 0} {not {> x 2}}}") => true)
(test (run "{with {c True} {if c {then-do {> 2 1}} {else-do 2}}}") => true)
(test (run "{with {foo {fun {x} {if {< x 2} {then-do x} {else-do {/ x 2}}}}} foo}") => (Fun 'x (If (Smaller (Id 'x) (Num 2)) (Id 'x) (Div (Id 'x) (Num 2)))))
(test (run "{with {foo {fun {y} {if {= x 12} {then-do x} {else-do {+ x 2}}}}} foo}") => (Fun 'y (If (Equal (Id 'x) (Num 12)) (Id 'x) (Add (Id 'x) (Num 2)))))
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `if' syntax in ")
(test (run "true") =error> "eval: free identifier: true")
(test (run "{< false 5}") =error> "eval: free identifier: false")
(test (run "{< False 5}") =error> "Num->number: expected a number, got: ")
(test (run "{if {* -3 3} {then-do 4} {else-do 5}}") => 5)
(test (run "{if {true} {then-do 4} {else-do 5}}") =error> "bad syntax in (true)")
(test (run "{not {fun {x} {+ x 3}}}") =error> "flang->bool: free identifier: ")
(test (run "{with {x 0} {if {* x 0} {then-do {+ 2 x}} {else-do x}}}") => 2)
(test (run "{with {x 1} {not {> x 2}}}") => true)
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `if' syntax in ")
(test (run "{< false 5}") =error> "eval: free identifier: ")
(test (run "{with {x 5} {with {x x} x}}") =>  5)
(test (run "{with {x 0} }") =error> "bad `with' syntax in ")
(test (run "{with {x 0} {if {> x 0} {/ 2 x} x}}") =error> "parse-sexpr: bad `if' syntax in")
(test (run "{with {x 3} {if {> x 0} {then-do {/ 2 x}} {else-do x}}}") => 2/3)
(test (run "{with {x 4} {if {< 0 x} {then-do {/ 2 x}} {else-do x}}}") => 1/2)
(test (run "{with {x 0} {if {= x 0} {then-do {+ 6 x}} {else-do x}}}") => 6)
(test (run "{with {x 2} {if {- x 0} {then-do {+ 2 x}} {else-do x}}}") => 4)
(test (run "{with {x 0} {if {+ x 0} {then-do {+ 7 x}} {else-do x}}}") => 7)
(test (run "{with {x 0} {if {* x 0} {then-do {+ 2 x}} {else-do x}}}") => 2)
(test (run "true") =error> "eval: free identifier:")

