#lang sicp
(#%require racket/trace)

;; Ex. 1.32
(define  (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
		(accumulate-rec combiner
				null-value
				term
				(next a)
				next
				b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a res)
    (if (> a b)
	res
	(iter (next a)
	      (combiner res
			(term a)))))
  (iter a null-value))

(define (id x) x)
(define (sum f a next b)
  (accumulate-iter + 0 f a next b))
(define (product f a next b)
  (accumulate-iter * 1 f a next b))
