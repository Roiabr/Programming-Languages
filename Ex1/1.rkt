#lang pl

#|
   Qusetion 1
               |#
#| 
  In this qusetion we need to find if there is a
  string in a list that has pl in her sufix
  the qusetion was very hard becuase most of my problems
  was bad sytenx and errors i didnt understand 
|#

#|
 this function get a string and check if pl
 is sufix of this string
|#
(: checkSuffix : String -> Boolean)
(define (checkSuffix S1)
  (let ([s1Length (string-length S1)]);;the length of the string
  (cond[(< s1Length 2) #f];;always false if small then "pl" length idea came from geeksforgeeks website
    [else(equal? (substring S1 (- s1Length 2) s1Length) "pl")])))


#|
 this function get a list of string and check if there is a word with sufix pl
 if yes - return the word
 otherwise return false
|#
(: plSuffixContained : (Listof String) -> (U String #f))
(define (plSuffixContained list)
  (cond
       [(null? list) #f]
       [(eq? (checkSuffix (first list)) #t) (first list)]
       [else  (plSuffixContained(rest list))]))


;;;;;;;;;; tests 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test(checkSuffix "" ) => #f)
(test(checkSuffix "mmmmm" ) => #f)
(test(checkSuffix "x" ) => #f)
(test( plSuffixContained '("x" "xox" "lol")) => #f)
(test( plSuffixContained '("hi" "my " "name" "is" "plpl")) => "plpl")
(test( plSuffixContained '("yyyt" "TplT" "plTT" "PlPl" "plplpl")) => "plplpl")
(test( plSuffixContained '("yyyt" "TplT" "" "PlPl" "plplpl")) => "plplpl")
(test( plSuffixContained '("" "T" "" "PPl" "p")) => #f)
(test( plSuffixContained '("pl" "plpl" "" "plplpl" "plplplpl")) => "pl")






#|
   Qusetion 2
               |#

#|
  In this Qusetion we need to write a polynom as a string 
  and also calculte the polynom with givin x
  the Qusetion was very hard for me and i had a lot of problems
|#
(: write-poly : (Listof Number)-> String)
;;This function get a list of numbers and return a string of a polynom
(define (write-poly lis)
  (let ([str  ""])
  (: helper : String (Listof Number) -> String)
  (define (helper str lis)
    (cond [(null? lis) str]
          [else (helper (string-append str (getPower (first lis) (- (length lis) 1) str )) (rest lis))]
    )
  )
 (helper str lis)
 )
)


(: getPower : Number Number String -> String)
;This function get the base and the power and return the number with
;;the x and the power(3x^ ..) as a string 
(define (getPower bas po st)
  (cond
    [(eq? po 0) (getSig bas)]
    [(eq? po 1) (string-append (getSig bas) "x")]
    [(eq? st "") (string-append (number->string bas) "x^" (number->string po))]
    [else (string-append (getSig bas) "x^" (number->string po))]
  )
)


(: getSig : Number -> String)
;this funtion get the base as a number and check if
;we need to add + before the number
(define (getSig bas)
  (cond
    [(> bas 0) (string-append "+" (number->string bas))]
    [else (number->string bas)]
    )
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;2.2;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#|
 In this section calculate the polynom with a givin x
 this section was a little bit easy since most of the hard word was done in 2.1
 i had a poblem with the expt function so i write the power function.
|#
(: compute-poly : Number (Listof Number)-> Number )
;;The function get a number(x) and a llist of number
;;and return the number that is the answer of the polynom and x
(define (compute-poly x lis)
  (: helper : Number (Listof Number)-> Number)
  (define (helper ans ls)
    (if(null? ls) ans
     (helper (+ ans (findPoly(first ls) x (- (length ls) 1))) (rest ls))
     )
    )
    (helper 0 lis)
  )
(: findPoly : Number Number Integer -> Number )
;;This function get 3 Numbers as input the base the x and the power
;;and return the Calculate the numbeר With x and power
(define (findPoly bas x pow)
  (cond
    [(and (eq? x 0) (> pow 0)) 0]
    [(eq? x 1) bas]
    [(eq? pow 0) bas]
    [(eq? pow 1)(* x bas)]
    [else  (* bas (expt x pow))]
    )
)

;;;;;;;;;;;;;; tests 2.1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (write-poly '(3 2 6)) => "3x^2+2x+6")
(test (write-poly '()) => "")
(test (write-poly '(7 8 9 10)) => "7x^3+8x^2+9x+10")
(test (write-poly '(-1 2 -3 4)) => "-1x^3+2x^2-3x+4")
(test (write-poly '(1 -2 3 -4)) => "1x^3-2x^2+3x-4")
(test (write-poly '(1 1 1)) => "1x^2+1x+1")
(test (write-poly '(-2 -2 -2)) => "-2x^2-2x-2")
(test (getPower 3 2 "" ) => "3x^2")
(test (getPower 3 1 "2x^2" ) => "+3x")
(test (getSig -1) => "-1")
(test (getSig 40) => "+40")

;;;;;;;;;;;;;;;;;;; test 2.2 ;;;;;;;;;;;;;;;;;;;;;

(test (findPoly 2 2 2) => 8)
(test (findPoly -2 1 2) => -2)
(test (findPoly -2 -2 2) => -8)

(test (compute-poly 2 '()) => 0)
(test (compute-poly 0 '(1 2 4)) => 4)
(test (compute-poly 2 '(-1 -2)) => -4)
(test (compute-poly -2 '(-1 -2)) => 0)
(test (compute-poly 1 '(4)) => 4)
(test (compute-poly 2 '(3 2 6)) => 22)
(test (compute-poly 3 '(4 3 -2 0)) => 129)
   

#|
   Qusetion 3.
                |#
#|
  In this question we asked to write the dataType - KeyStack.
  KeyStack has a key(Symbol)- index in stack, value(String) and KeyStack(EmptyKS\Push).
  EmptyKS is the constructor and does not recieves any inputs.
  Push is a recursive keyStack that get all the inputs for the data KeyStack.(constructor)
  The hard part was to understand how the constructor works and how to work with the constructor to write the functions
|#

(define-type KeyStack
  [EmptyKS]
  [Push Symbol String KeyStack])


#|
  This function recieves Symbol and KeyStack as inputs nad
  searches for the String with this symbol(as an index),
  if the string does not exists or the given KeyStack is an EmptyKS return false,
  else it will find the keyStack with this symbol by recursivelly calling this function with rest of the ks using push constructor.
|#
(: search-stack : Symbol KeyStack -> (U String Boolean))
(define (search-stack s ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push symb str st)
     (cond [(equal? s symb) str]
           [else (search-stack s st)])]))

#|
 This function recieves KeyStack as an input and pops its keyed-stack. 
 If it stack is empty - returns #f,
 else, will return the rest(KeyStack) with Push constructor.
|#
(: pop-stack : KeyStack -> (U Boolean KeyStack))
(define (pop-stack ks)
  (cases ks
    [(EmptyKS) #f]
    [(Push symb str ks) ks]))

;;;;;;;;;;;;;;;;; tests 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(test (EmptyKS) => (EmptyKS))
(test (Push 'b "B" (Push 'a "A" (EmptyKS))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))) => (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS)))))
(test (Push 'y "yes" (Push 'w "we" (Push 'c "can" (EmptyKS)))) => (Push 'y "yes" (Push 'w "we" (Push 'c "can" (EmptyKS)))))
(test (search-stack 'a (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => "AAA")
(test (search-stack 'c (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => #f)
(test (search-stack 'y (Push 'y "yes" (Push 'w "we" (Push 'c "can" (EmptyKS))))) => "yes")
(test (pop-stack (EmptyKS)) => #f)
(test (pop-stack (Push 'a "AAA" (Push 'b "B" (Push 'a "A" (EmptyKS))))) => (Push 'b "B" (Push 'a "A" (EmptyKS))))
(test (pop-stack (Push 'y "yes" (Push 'w "we" (Push 'c "can" (EmptyKS))))) => (Push 'w "we" (Push 'c "can" (EmptyKS))))


#|
   Qusetion 4
               |#

#|
 the functions both work recursivly way
 if the number(n) is odd so number - 1 is even(work the same if number is even)
 and the run stop when n is zero
 so if the number is 3 the next run is 2(is-even) 1 (is-odd) 0 (is-even) return true
 and the same way with even number.
|#
(: is-odd? : Natural -> Boolean)
;; this functiom get as an input netural number and returns a boolean
;; The function's porpuse is to check if the given number is odd number by sending to is-even minus 1 
(define (is-odd? x)
(if (zero? x)
false
(is-even? (- x 1))))

(: is-even? : Natural -> Boolean)
;; this functiom get as an input netural number and returns a boolean
;; The function's porpuse is to check if the given number is even number by sending to is-odd? minus 1
(define (is-even? x)
(if (zero? x)
true
(is-odd? (- x 1))))

;; tests --- is-odd?/is-even?
(test (not (is-odd? 12)))
(test (is-even? 12))
(test (not (is-odd? 0)))
(test (is-even? 0))
(test (is-odd? 1))
(test (not (is-even? 1)))

(: every? : (All (A) (A -> Boolean) (Listof A) -> Boolean))
;;This function get as an input a function that get A of any kind(Listof A)
;; and a list A and all A has to be as the same type All ((A) (A -> Boolean))
(define (every? pred lst)
(or (null? lst)
(and (pred (first lst))
(every? pred (rest lst)))))

;; An example for the usefulness of this polymorphic function
(: all-even? : (Listof Natural) -> Boolean)
;; the function gets a list of natural number an return boolean
;; the function check if all list is even number by using the every function
(define (all-even? lst)
(every? is-even? lst))

;; tests
(test (all-even? null))
(test (all-even? (list 0)))
(test (all-even? (list 2 4 6 8)))
(test (not (all-even? (list 1 3 5 7))))
(test (not (all-even? (list 1))))
(test (not (all-even? (list 2 4 1 6))))


(: every2? : (All (A B) (A -> Boolean) (B -> Boolean) (Listof A) (Listof B) -> Boolean))
;;the function recieves: function with A as input and boolean as output and function with B as input and boolean as output,
;; and list of A's, list of B's. Returns boolean.
;; and for all of this  A's B's need to be of the same type.
;;The two functions returns boolean for some condition ((A -> Boolean)(B -> Boolean) )
(define (every2? pred1 pred2 lst1 lst2)
(or (null? lst1) ;; both lists assumed to be of same length
(and (pred1 (first lst1))
(pred2 (first lst2))
(every2? pred1 pred2 (rest lst1) (rest lst2)))))
    