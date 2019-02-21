#lang sicp
(#%require racket/trace)

;; Ex. 1.40
(define (newtons-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define dx 0.00001)

(define (fixed-point g initial-guess)
  (define (try guess)
    (let ((next (g guess)))
      (if (good-enough guess next)
	  next
	  (try next))))
  (try initial-guess))

(define (good-enough a b)
  (< (abs (- a b)) dx))

(define (newtons-method f guess)
  (fixed-point (newtons-transform f) guess))


;; Ex. 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))
(define (square x) (* x x))
(define (cube x) (* x (square x)))

(newtons-method (cubic 4.0 (- 8.0) 7.0) 1.0)
