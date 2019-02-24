#lang sicp
(#%require racket/trace)

;; Ex. 1.43
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (square x) (* x x))

;; naive implementation: linear recursion
(define (repeated-naive f n)
  (if (= n 1)
      f
      (compose f (repeated-naive f (- n 1)))))

((repeated-naive square 2) 5) ;; 625
((repeated-naive inc 3) 10) ;; 13

;; log recursion
(define (even? n)
  (= (remainder n 2) 0))

(define (repeated f n)
  (cond ((= n 1) f)
	((even? n)
	 (repeated (compose f f) (/ n 2)))
	(else (compose f (repeated f (- n 1))))))

((repeated square 2) 5) ;; 625
((repeated inc 3) 10) ;; 13
