#lang sicp
(#%require racket/trace)

;; Ex. 1.31
;; linear recursive product
(define (product-rec f a next b)
  (if (> a b)
      1
      (* (f a)
	 (product-rec f (next a) next b))))

;; iterative product
(define (product-iter f a next b)
  (define (iter a result)
    (if (> a b)
	result
	(iter (next a) (* result (f a)))))
  (iter a 1))

(define (id x) x)

(define (factorial x)
  (product-iter id 1 inc x))

(define (square x) (* x x))
(define (plus2 x) (+ 2 x))
(define (pi iter)
  (* (/ 8.0 iter)
     (/ (product-iter square 4.0 plus2 iter)
	(product-iter square 3.0 plus2 (- iter 1)))))

(pi 171)
