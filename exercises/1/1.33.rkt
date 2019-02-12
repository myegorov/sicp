#lang sicp
(#%require racket/trace)

;; Ex. 1.33
(define (filtered-accumulate combiner
			     null-value
			     term
			     a
			     next
			     b
			     predicate)
  (define (iter a res)
    (if (> a b)
	res
	(iter (next a)
	      (if (predicate a)
		  (combiner res
			    (term a))
		  res))))
  (iter a null-value))


(define (sum-squares-primes a b)
  (filtered-accumulate +
		       0
		       square
		       a
		       inc
		       b
		       prime?))

(define (id x) x)
(define (prod-all-pos-relatively-prime n)
  (define (relatively-prime? x)
    (= (gcd x n) 1))
  (filtered-accumulate *
		       1
		       id
		       1
		       inc
		       n
		       relatively-prime?))

;; from Ex. 1.22
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))
(define (square x) (* x x))

(define (smallest-divisor n)
  (define (test-smallest-divisor n test)
    (cond ((> (square test) n) n)
	  ((= (remainder n test) 0) test)
	  (else (test-smallest-divisor n
;;				       (next test)))))
  				       (+ test 1)))))
  (test-smallest-divisor n 2))

;; naive prime check
(define (prime? x)
  (= x (smallest-divisor x)))

(define (gcd a b)
  (define (gcd-helper x y)
    (if (= y 0)
	x
	(gcd-helper y (remainder x y))))
  (if (> a b)
      (gcd-helper a b)
      (gcd-helper b a)))
