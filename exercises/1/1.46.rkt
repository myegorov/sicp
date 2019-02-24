#lang sicp
(#%require racket/trace)

;; Ex. 1.46
(define (iterative-improve good-enough? improve-guess)
  (lambda (initial-guess)
    (define (iter guess)
       (let ((next (improve-guess guess)))
	  (if (good-enough? guess)
	     next
	     (iter next))))
    (iter initial-guess)))

(define (sqrt x)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- (square guess) x)) tolerance))
    (lambda (guess)
      (average guess (/ x guess))))
   1.0))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) tolerance))
    f)
   first-guess))

;; auxiliary functions
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (average a b)
  (/ (+ a b) 2.0))
(define tolerance 0.0000001)
(define (square x) (* x x))

