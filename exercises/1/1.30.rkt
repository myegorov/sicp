#lang sicp
(#%require racket/trace)

;; Ex. 1.30: iterative sum
(define (sum f a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (f a)))))
  (iter a 0))

(define (naive-integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2.0)) add-dx b)))

(define (cube x)
  (* x x x))

(naive-integral cube 0 1 0.01)

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
