#lang sicp
(#%require racket/trace)

(define (sum f a next b)
  (if (> a b)
      0
      (+ (f a)
	 (sum f (next a) next b))))

(define (naive-integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2.0)) add-dx b)))

(define (cube x)
  (* x x x))

(naive-integral cube 0 1 0.01)

;; Ex. 1.29: Simpson's rule
;; Simpson's rule yields exact result for polynomial of degree 3 or less
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (factor iter)
    (cond ((or (= iter 0) (= iter n)) 1)
	  ((even? iter) 2)
	  (else 4)))
  (define (g iter)
    (* (f (+ a (* iter h)))
       (factor iter)))
  (define (inc x) (+ 1 x))
  (* (sum g 0.0 inc n)
     (/ h 3.0)))

(simpson-integral cube 0.0 1.0 100)
