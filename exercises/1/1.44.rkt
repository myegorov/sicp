#lang sicp
(#%require racket/trace)

;; Ex. 1.44
(define dx 0.0001)
(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (+ x dx))
          (f (- x dx)))
       3.0)))

(define (even? n)
  (= (remainder n 2) 0))

(define (repeated f n)
  (cond ((= n 1) f)
	((even? n)
	 (repeated (compose f f) (/ n 2)))
	(else (compose f (repeated f (- n 1))))))
       
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (smooth-n-fold f n)
  ((repeated smooth n) f))
