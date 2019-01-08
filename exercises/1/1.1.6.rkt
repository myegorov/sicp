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

(test 0 (p)) ; => infinite loop if applicative-order evaluation; 0 if normal-order
(test 0 (/ 1 0)) ; => division-by-zero error if applicative-order evaluation; 0 if normal-order
