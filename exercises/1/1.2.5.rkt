#lang sicp
(#%require racket/trace)

;; Euclid's GCD algorithm

(define (gcd a b)
  (define (gcd-helper x y)
    (if (= y 0)
	x
	(gcd-helper y (remainder x y))))
  (if (> a b)
      (gcd-helper a b)
      (gcd-helper b a)))

