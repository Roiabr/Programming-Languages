#lang pl

#| Please complete the missing rules below  
<SOL> :: = { <NumList> }
        |  { scalar-mult <num> <SOL> };; need to get number(scalar) and SOL
        |  { intersect <SOL> <SOL>}  ;;need to do the op one two SOL
        |  { union <SOL> <SOL> }  ;;need to do the op one two SOL
        |  <id>
        |  { with {<id> <SOL> } <SOL> } ;; this should be a syntactic sugar
        |  { fun { <id> <id> } <SOL> } ;; a function must have exactly two formal parameters
        |  { call-static <SOL> <SOL> <SOL> } ;; extends closure environment
        |  { call-dynamic <SOL> <SOL> <SOL> } ;; extends current environment

<NumList> :: =  λ | <num> <NumList> ;; where λ stands for the empty word, i.e., { } is the empty set

;; where <num> is any expression identified by Racket as a Number
;; and <id> is any expression such that Racket identifies '<id> as a symbol
 
|#


;; -----------------------------------------------------
;; The abstract syntax tree SOL
(define-type SET = (Listof Number))
(define-type SOL
  ;; Please complete the missing parts -- you are NOT allowed to use additional variants (constructors)
  [Set  SET]
  [Smult Number SOL] ;; same as bnf
  [Inter SOL SOL]   ;; same as bnf
  [Union SOL SOL]   ;; same as bnf
  [Id    Symbol]
  ;;    [With  Symbol SOL SOL] -- not to be used, syntactic sugar for ...
  [Fun   Symbol Symbol SOL]
  [CallS SOL SOL SOL]
  [CallD SOL SOL SOL])

;; ----------------------------------------------------
;; Operations on SETs
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

(: ismember? : Number SET  -> Boolean)
(define (ismember? n l)
  (cond [(null? l) #f]
        [(= n (first l)) #t]
        [else (ismember? n (rest l))]))

(test (not (ismember? 1 '(3 4 5))))
(test (not (ismember? 1 '( 3 2 3 5 6))))
(test (ismember? 1 '(3 4 5 1 3 4)))
(test (ismember? 1 '(1)))

;; the fuction get a SET and remove the duplicates from the set and return a sort set
(: remove-duplicates : SET  -> SET)
(define (remove-duplicates l)
  (cond [(or (null? l) (null? (rest l))) l]
        [(ismember? (first l)(rest l))(remove-duplicates (rest l))]
        [else (cons (first l) (remove-duplicates (rest l)))]))

(test (remove-duplicates '(3 4 5 5)) => '(3 4 5) )
(test (remove-duplicates'( 3 2 3 5 6)) => '(2 3 5 6) )
(test (remove-duplicates '(3 4 5 1 3 4)) => '(5 1 3 4))
(test (remove-duplicates '(1)) => '(1) )

;; the function get a set and remove duplicte after we sort the set
(: create-sorted-set : SET -> SET)
(define (create-sorted-set l)
  (remove-duplicates (sort l <))) ;; call remove-duplicates after sort to create real set

(test (create-sorted-set '(3 4 5 5)) => '(3 4 5) )
(test (create-sorted-set '( 3 2 3 5 6)) => '(2 3 5 6) )
(test (create-sorted-set '(3 4 5 1 3 4)) => '(1 3 4 5))
(test (create-sorted-set '(1)) => '(1) )
(test (create-sorted-set '()) => '() )   

;; the function get two sets and return a one union set (sorted wirhout duplicate)
(: set-union : SET SET -> SET)
(define (set-union A B)
  (create-sorted-set (append A B))) ;; use append for union the two sets

(test (set-union '(3 4 5 5)  '(3)) => '(3 4 5) )
(test (set-union '( 3 2 3 5 6) '(6 6)) =>'(2 3 5 6) )
(test (set-union '(3 4 5 1 3 4) '()) => '(1 3 4 5))
(test (set-union '(1)  '()) => '(1) )
(test (set-union '() '()) => '() )  
    
;; the function get two sets and return a one interset set (sorted wirhout duplicate)
(: set-intersection : SET SET -> SET)
(define (set-intersection A B)
  (: mem-filter : Number -> Boolean)
  (define (mem-filter n)
    (ismember? n A))
  ( create-sorted-set (filter mem-filter  B))) ;; send filter the mem-filter to get the intersect between two sets

(test (set-intersection '(3 4 5 5)  '(3)) => '(3) )
(test (set-intersection '( 3 2 3 5 6) '(6 6)) =>'(6) )
(test (set-intersection '(3 4 5 1 3 4) '()) => '())
(test (set-intersection '(1)  '()) => '() )
(test (set-intersection '() '()) => '() ) 


;; ---------------------------------------------------------
;; Parser
;; Please complete the missing parts, and add comments (comments should specify 
;; choices you make, and also describe your work process). Keep your code readable. 
(: parse-sexpr : Sexpr -> SOL)
;; to convert s-expressions into SOLs
(define (parse-sexpr sexpr)
  (match sexpr
    [(list (number: ns) ...) (Set (create-sorted-set ns))] ;; sort and remove-duplicates and and Set to return SOL
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (CallS (Fun name name (parse-sexpr body))(parse-sexpr named) (parse-sexpr named))] ;;; there is no With constructor replace with existing constructors SO i used CallS constructer
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name1) (symbol: name2)) body)
        (if (eq? name1 name2)
            (error 'parse-sexpr"fun' has a duplicate param name in ~s" sexpr ) ;; cannot use the same param name twice
            (Fun name1 name2 (parse-sexpr body)))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'scalar-mult (number: sc) rhs) (Smult sc (parse-sexpr rhs))]
    [(list 'intersect lhs rhs) (Inter (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'union lhs rhs) (Union (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call-static fun arg1 arg2) (CallS (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;; CallS with 3 args (SOL SOL SOL)
    [(list 'call-dynamic fun arg1 arg2) (CallD (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))] ;; CallD with 3 args (SOL SOL SOL
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

    


(: parse : String -> SOL)
;; parses a string containing a SOL expression to a SOL AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

  
(test (parse "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => (Set '(1 2 3 4)))
(test (parse "{union {1 2 3}}") =error> "parse-sexpr: bad syntax in (union (1 2 3))")
(test (parse "{union {1 2 3} {4 2 3}}") => (Union (Set '(1 2 3)) (Set '(2 3 4))))
(test (parse "{fun {x x} x}") =error> "parse-sexpr: fun' has a duplicate param name in (fun (x x) x)")
(test (parse "{with {{1} {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") =error> "parse-sexpr: bad `with' syntax in (with ((1) (intersect (1 2 3) (4 2 3))) (call-static (fun (x y) (union x S)) (scalar-mult 3 S) (4 5 7 6 9 8 8 8))")

(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}") 
      =>
      (CallS (Fun 'S
                  'S
                  (CallS (Fun 'x 'y (Union (Id 'x) (Id 'S))) 
                         (Smult 3 (Id 'S)) 
                         (Set '(4 5 6 7 8 9))))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))
             (Inter (Set '(1 2 3)) (Set '(2 3 4)))))


(test (parse "{with {S {intersect {1 2 3} {4 2 3}}}
              {fun {x} S}}")
      =error> "parse-sexpr: bad `fun' syntax in (fun (x) S)") ;; functions require two formal parameters

  

;;-----------------------------------------------------
;; Evaluation 
#|
------------------------------------------------------
Evaluation rules:
    ;; Please complete the missing parts in the formal specifications below

    eval({ N1 N2 ... Nl }, env)  =  (sort (create-set (N1 N2 ... Nl)))
                               where create-set removes all duplications from
                              the sequence (list) and sort is a sorting procedure
    eval({scalar-mult K E},env) =   (K*N1 K*N2 ... K*Nl) if (N1 N2 ... Nl) = eval(E,env) is a sorted set
                                = error! otherwise (if S is not a sorted set)
    eval({intersect E1 E2},env) = (sort (create-set (set-intersection (eval(E1,env) , eval(E2,env))))
                                    if both E1 and E2 evaluate to sorted sets
                                = error! otherwise
    eval({union E1 E2},env) = (sort (create-set (eval(E1,env) , eval(E2,env))))
                                  if both E1 and E2 evaluate to sorted sets
                             = error! otherwise
    eval({with {x E1} E2},env) = eval(E2,extend(x,eval(E1,env),env))
    eval({fun {x1 x2} E},env)  = <{fun {x1 x2} E}, env>
    eval({call-static E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env), extend(x1,eval(E1,env),envf) 
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise
    eval({call-dynamic E-op E1 E2},env)
             = eval(Ef,extend(x2,eval(E2,env), extend(x1,eval(E1,env),env)
                               if eval(E-op,env) = <{fun {x1 x2} Ef}, envf>
             = error!          otherwise

|#

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [SetV SET]
  [FunV Symbol Symbol SOL ENV])

(: lookup : Symbol ENV -> VAL)
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))


;; Auxiliary procedures for eval 
;; Please complete the missing parts, and add comments (comments should specify 
;; the role of each procedure, but also describe your work process). Keep your code readable. 

;; the function get a set as VAL and return the set as SET
(: SetV->set : VAL -> SET)
(define (SetV->set v)
  (cases v
    [(SetV S) S]
    [else(error 'SetV->set "expects a set, got: ~s" v)]))

;; The funcion get the number(skalar) and a val(set) and return VAL that the set is mul every number is the list with the skalar
(: smult-set : Number VAL -> VAL)
(define (smult-set n s)
  (: mult-op : Number -> Number)
  (define (mult-op k)
    (* k n))
  (SetV (map mult-op (SetV->set s)))) ;; map run the op(mult-op) on every number in set and then wrap with SetV for return VAL

(: set-op : (SET SET -> SET ) VAL VAL -> VAL )
;; gets a binary SET operator, and uses it within a SetV
;; wrapper
(define (set-op op val1 val2)
  (SetV (op (SetV->set val1) (SetV->set val2)))) 

;;---------  the eval procedure ------------------------------
;; Please complete the missing parts, and add comments (comments should specify 
;; the choices you make, and also describe your work process). Keep your code readable. 
(: eval : SOL ENV -> VAL)
;; evaluates SOL expressions by reducing them to set values
(define (eval expr env)
  (cases expr
    [(Set S) (SetV S)] ;; wrapp with SetV for VAL
    [(Smult n set) (smult-set n (eval set env))] ;; n is number then evel the set with the env
    [(Inter l r) (set-op set-intersection  (eval l env) (eval r env))] ;;right op with evel the set with the env on two sets
    [(Union l r)(set-op set-union (eval l env) (eval r env))] ;;right op with evel the set with the env on two sets
    [(Id name) (lookup name env)]
    [(Fun bound-id1 bound-id2 bound-body)
     (FunV bound-id1 bound-id2 bound-body env)] ;; return the fun
    [(CallS fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body ;; evel on the body
                (Extend bound-id2 (eval arg-expr2 env) ;; extend for the free paramters
                        (Extend bound-id1 (eval arg-expr1 env) f-env)))]  ;; extend for the free paramters with the f-env to be a static call (not env)
         [else (error 'eval "`call-static' expects a function, got: ~s"
                      fval)]))]
   
    [(CallD fun-expr arg-expr1 arg-expr2)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id1 bound-id2 bound-body f-env)
          (eval bound-body ;; evel on the body
                (Extend bound-id2 (eval arg-expr2 env) ;; extend for the free paramters
                        (Extend bound-id1 (eval arg-expr1 env) env)))] ;; extend for the free paramters with the env to be a dyanmic call (not f-env)
         [else (error 'eval "`call-dynamic' expects a function, got: ~s"
                      fval)]))]))
;; The funcrion create a env for the language  - the dunction impliment the pair using the words first second cons and with EmptyEnv
;; to extend the pramter to the env
(: createGlobalEnv : -> ENV)
(define (createGlobalEnv)
  (Extend 'second (FunV 'params1 'params2 (CallS (Id 'params1) (Fun 'fir 'sec (Id 'sec)) (Set '())) (EmptyEnv))
          (Extend 'first (FunV 'params1 'params2 (CallS (Id 'params1) (Fun 'fir 'sec (Id 'fir)) (Set '())) (EmptyEnv))
                  (Extend 'cons (FunV 'first 'second (Fun 'args1 'args2 (CallS (Id 'args1) (Id 'first) (Id 'second))) (EmptyEnv))
                          (EmptyEnv))))) 

(: run : String -> (U SET VAL))
;; evaluate a SOL program contained in a string
(define (run str)
  (let ([result (eval (parse str) (createGlobalEnv))]) ;; we want to send the env so we call create global env
    (cases result
      [(SetV S) S];; return the set
      [else result])));; retrun a Fun(VAL)


(test (run "{1 2 3  4 1 4  4 2 3 4 1 2 3}") => '(1 2 3 4))
(test (run "{1 }") => '(1))
(test (run "{}") => '())
(test (run "{union {1 2 3} {4 2 3}}") => '(1 2 3 4))
(test (run "{union {} {}}") => '())
(test (run "{union {1} {1 2}}") => '(1 2))
(test (run "{union {0} {2 1}}") => '(0 1 2))
(test (run "{intersect {1 2 3} {4 2 3}}") => '( 2 3))
(test (run "{intersect {1 2 3} {1 2 3}}") => '(1 2 3))
(test (run "{intersect {1 2 3} {}}") => '())
(test (run "{intersect {} {}}") => '())
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(2 3 6 9))
(test (run "{with {S {union {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(1 2 3 4 6 9 12))
(test (run "{with {S {union {1 2 3} {4 2 3}}}
                 {call-static {fun {x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(3 4 5 6 7 8 9 12))
(test (run "{with {S {intersect {1 2 3} {4 2 3}}}
                 {call-static {fun
{x y} {union x y}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}")
      => '(4 5 6 7 8 9))

(test (run "{with {p {call-static cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-static first p {}}
                                  {call-static second p {}}}}
                 {call-static {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))



(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =>  '(2 3 6 9))
(test (run "{with {p {call-dynamic cons {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first c {}}
                                  {call-dynamic second c {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =error>  "lookup: no binding for c" )
(test (run "{call-static {1} {2 2} {}}")
      =error> "eval: `call-static' expects a function, got: #(struct:SetV (1))")
(test (run "{with {g {call-dynamic {1 2 3} {1 2 3} {4 2 3}}}
              {with {S {intersect {call-dynamic first p {}}
                                  {call-dynamic second p {}}}}
                 {call-dynamic {fun {x y} {union x S}}
                              {scalar-mult 3 S}
                              {4 5 7 6 9 8 8 8}}}}")
      =error> "eval: `call-dynamic' expects a function, got: #(struct:SetV (1 2 3)")


#| --------------- Q5 ----------------------------------------
 1. I used lesson summaries and assignments that I wrote because questions 1-4 was like the assignments just with another language(SOL).
    The truth that question 5 was really hard and I had to see the lesson again with the summaries to understand the pair and global env again - also i used the tirgul record(#11) that Tom send.
    Im used the tests in 5.b to complete the createGlobalEnv and i just copy the result of the test for  every Extend
 2. Every define-type we write in our language ( SOL - language for sets , SET - the define for set , ENV - for the envierment language(dynamic\static) , VAL - our lang havs only 2 option sets or functions ) - all are new type we defined using the define-type
 3. I call the Fun with name twice (Fun name(symbol) name(symbol)) in this i used the the call-static for couple resons - first i didnt know very good the dynamic call so was affrid that i will need to change a lot of things for the parser to work
    second i know that when i used the static all the free pramters is valid when we define the function and i remmbered in the class eran said its better implementaion for language
 4. The tail recusion function is set-intersection and ismember?. The tail function is better becuase in tail we call last time then recusion function becuase the stop is when we get to the acc but in regular
    recursion we will go back until we get to null or 0, so we used less memory and we can get stack overflow in regular recusion
 5. I only used call static for all the global env(first second and cons)- again i wasnt sure if and how i need to use the dynamic and i remmber how to work with the static that i need to sent the empty env every call with the function
    and when i change the CallS to CallD i got the error - lookup: no binding for second so i thought there is a problem with the dynamic that biding when call the function, so i used CallS.
 6. Im not sure but when i change the CallS to CallD i got the error - lookup: no binding for second so i thought there is a problem with the dynamic that biding when call the function and no args with name(symbol) is in the env so it cant binding
|#

