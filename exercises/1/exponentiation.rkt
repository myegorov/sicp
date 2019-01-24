#lang sicp
(#%require racket/trace)

;; translate definition of exponentiation
;; linear recursive version
(define (pow-rec b n)
  (if (= n 0)
      1
      (* b (pow-rec b (- n 1)))))

;; O(n) time, O(1) space iterative version
(define (pow-iter b n)
  (define (helper b counter product)
    (if (= counter 0)
	product
	(helper b (dec counter) (* b product))))
  (helper b n 1))

;; O(log n) time, O(log n) space version
(define (square x) (* x x))
(define (pow-log b n)
  (cond ((= n 0) 1)
	((= (remainder n 2) 0) (square (pow-log b (/ n 2))))
	(else (* b (pow-log b (- n 1))))))

;; Ex. 1.16
;; O(log n) time, O(1) space version
(define (pow-log-iter b n)
  (define (helper b counter product)
    (cond ((= counter 0) product)
	  (else (helper b
			(- counter 1)
			(* product (square b))))))
  (if (= (remainder n 2) 0)
      (helper b (/ n 2) 1)
      (helper b (/ (- n 1) 2) b)))
