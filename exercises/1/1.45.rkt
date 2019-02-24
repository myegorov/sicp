#lang sicp
(#%require racket/trace)

;; Ex. 1.45
(define (average-damp f)
  (lambda (x) 
    (average x (f x))))
(define (average a b)
  (/ (+ a b) 2.0))

(define tolerance 0.0000001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) 
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


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

(define (square x) (* x x))
(define (pow x n)
  (define (iter m res)
    (cond ((= m 0) 1.0)
	  ((even? m)
	   (square (iter (/ m 2) res)))
	  (else (* x (iter (- m 1) res)))))
  (iter n x))

(define (root n x)
  (fixed-point 
   ((repeated average-damp (- n 1))
    (lambda (y) (/ x (pow y (- n 1)))))
   1.0))

(define (sqrt x)
  (fixed-point 
   (average-damp 
    (lambda (y) (/ x y)))
   1.0))

(root 5 32) ; => 2.0
