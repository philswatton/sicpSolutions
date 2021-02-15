#lang sicp


; Exercise 1.2
(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               (/ 4
                  5)))))
   (* 3
      (- 6
         2)
      (- 2
         7)))



; Excerise 1.3 - function that takes 3 numbers, returns the sum of the squares of the largest two

; if a > b is true, check b > c, if this is true, square a and b, if false, square a and c
; if false, we *know* a < b
; if a > c is true, square a and b, if false, square b and c

(define (square x) (* x x))

(define (sumSquareLarge a b c)
  (if (> a b)
      (if (> b c)
          (+ (square a) (square b))
          (+ (square a) (square c)))
      (if (> a c)
          (+ (square a) (square b))
          (+ (square b) (square c)))))

(sumSquareLarge 1 2 3)
(sumSquareLarge 3 2 1)
(sumSquareLarge 2 1 3)




; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
; explanation:
; if b is positive, add and b. If b is negative, substract b from a, thus defacto meaning the function always adds the absolute of b to a




; Exercise 1.5

; With applicative order will continuously evaluate, because (p) will never stop evaluating to itself
; Conversely, when normal order is used will return 0, because (p) is never needed and thus never evaluated





; Excercise 1.6

; Default if evaluation (special form so not applicative order): evalue predicate. If true, evaluate consequent. Else, evaluate alternative.
; General form written by user will evaluate both at the time when used in the recursive Newton function. Because of this feature, will never stop evaluating because of the recursive call





; Excerise 1.7 - square root approximation where evaluation stops when the change is a fraction of the guess

#|
Newton Method:
- start with a guess as to the square root of number x
- get quotient of x/guess
- get average of the quotient and the guess
- repeat where that result is the new guess

in this implementation:
- stop when the change from one guess to another is a fraction of the guess|#

(define (average a b)
  (/ (+ a b) 2))

(define (update guess x)
  (average (/ x guess) guess))

(define (sqrt-iter guess x)
  (if (< (/ (abs (- (update guess x) guess)) guess) 0.0001) ;want to stop when the *change* between guesses is v small. so absolute of updated guess - current guess is divided by the guess
      guess
      (sqrt-iter (update guess x) x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

#|
(update 1.0 2.0)
(update 1.5 2.0)
(update (update 1.5 2.0) 2.0)
(/ (update 1.5 2.0) (update 1.0 2.0))|#
(sqrt 4.0)
;(sqrt 0.0004)
;(sqrt 1.0)
;(sqrt 0.001)
;(sqrt 10000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000)
; at this point can't really tell the difference in performances without manually working out the error and I'm too lazy for that :(







; Excercise 1.8

#| Newton's method for Cube roots

If y is an approximation to the cube root of x, then a better approximation is given by ((x/y^2) + 2y)/3

Implemnt a cube root procedure anologous to the square root procedure

- Steps:
- First, check if change is a fraction of the current guess
- If yes, return the present value
- If no, update guess, recursive call |#

(define (improve guess x)
  (/ (+ (/ x
           (* guess guess))
        (* 2 guess))
     3))

(define (cube-iter guess x)
  (if (< (/ (abs (- guess (improve guess x))) guess) 0.0001)
      guess
      (cube-iter (improve guess x) x)))

(define (croot x)
  (cube-iter 1.0 x))

(croot 27.0)
(croot 125.0)



      
