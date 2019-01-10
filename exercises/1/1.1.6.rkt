#lang sicp

;; Ex. 1.1
10 ; => 10
(+ 5 3 4) ; => 12
(- 9 1) ; => 8
(/ 6 2) ; => 3
(+ (* 2 4) (- 4 6)) ; => 6
(define a 3) ; => void
(define b (+ a 1)) ; => void
(+ a b (* a b)) ; => 19
(= a b) ; => #f
(if (and (> b a) (< b (* a b)))
    b
    a) ; => 4
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25)) ; => 16
(+ 2 (if (> b a) b a)) ; => 6
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1)) ; => 16

;; Ex. 1.2
(/ (+ 5 4 (- 2
	     (- 3
		(+ 6 4/5))))
   (* 3
      (- 6 2)
      (- 2 7)))

;; Ex. 1.3
(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))
(define (<= x y) (not (> x y)))
(define (min x y z)
  (cond ((and (<= x y) (<= x z)) #t)
	(else #f)))

(define
  (sum-of-squares-of-max-2 x y z)
  (cond ((min x y z) (sum-of-squares y z))
	((min y x z) (sum-of-squares x z))
	(else (sum-of-squares x y))))

;; Ex. 1.4
(define
  (a-plus-abs-b a b)
  ((if (< b 0) - +) a  b))

;; Ex. 1.5
(define (p) (p))

(define (test a b)
  (if (= a 0) 0 b))

;; (test 0 (p)) ; => infinite loop if applicative-order evaluation; 0 if normal-order
;; (test 0 (/ 1 0)) ; => division-by-zero error if applicative-order evaluation; 0 if normal-order


;; Ex. 1.6
;; new-if is now implemented using cond, rather than as a
;; special form. Therefore, both the then- and else-clauses
;; will be expanded before cond may return the value. The else-
;; clause will then lead to an infinite loop (recursively
;; calling sqrt-iter). The guess will eventually meet and
;; exceed the tolerance, but the base case (then-clause) will
;; never preempt evaluating the looping case.

;; Ex. 1.7
;; (define (good-enough? guess x)
;;   (< (abs (- (square guess) x)) 0.001))
;; For any sqrt substantially (say, an order of magnitude)
;; smaller than tolerance (here 0.001), the guess will be off.
;; I.e. TOL bounds the expected error if hardcoded.
;; For large numbers (orders of magnitude larger than TOL),
;; setting the TOL arbitrarily to 0.001 will lead to unnecessary
;; calculations. Worse yet, limited precision may lead infinite looping
;; (no progress is made in subsequent cycles because TOL falls below 
;; precision threshold).

;; implementing relative tolerance
(define
  (abs x)
  (if (< x 0)
      (- x)
      x))
(define
  (about-right? a b tol)
  (< (abs (- a b)) (* tol b)))
(define TOL 0.001)
(define
  (improve guess x)
  (/ (+ guess (/ x guess)) 2.0))
(define
  (sqrt-iter x guess)
  (if (about-right? (square guess) x TOL)
      guess
      (sqrt-iter x (improve guess x))))
(define
  (sqrt x)
  (sqrt-iter x 1.0))

;; Ex. 1.8
;; naive implementation with absolute tolerance
(define
  (mean3 a b c)
  (/ (+ a b c) 3.0))
(define
  (cube x)
  (* x (square x)))
(define
  (good-enough? x guess)
  (< (abs (- x (cube guess))) TOL))
(define
  (improve3 guess x)
  (mean3 (/ x (square guess)) guess guess))
(define
  (cube-root-iter x guess)
  (if (good-enough? x guess)
      guess
      (cube-root-iter x (improve3 guess x))))
(define
  (cube-root x)
  (cube-root-iter x 1.0))
