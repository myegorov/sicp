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

;; Ex. 1.17
;; define recursive multiplication
;; O(log n) in both space and time
(#%require scheme/fixnum)
;; use bitwise shift for double/halve
(define (double x) (fxlshift x 1))
(define (halve x) (fxrshift x 1))
(define (mult-rec x y)
  (cond ((or (= x 0) (= y 0)) 0)
	((= y 1) x)
	((= (remainder y 2) 0)
	 (mult-rec (double x) (halve y)))
	(else (+ x (mult-rec x (- y 1))))))

;; Ex. 1.18
;; define iterative multiplication
;; O(log n) time, O(1) space
(define (mult-iter x y)
  (define (helper x y acc)
    (cond ((or (= x 0) (= y 0)) x)
	  ((= y 1) (+ x acc))
	  ((= (remainder y 2) 0)
	   (helper (double x) (halve y) acc))
	  (else (helper x (- y 1) (+ x acc)))))
  (helper x y 0))
