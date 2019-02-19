#lang sicp
(#%require racket/trace)

(define (display-all . xs)
  (for-each display xs))

;; Ex. 1.36
(define (fixed-point f init-guess)
  (define tolerance 0.00001)
  (define (close-enough a b)
    (< (abs (- a b)) tolerance))
  (define (try guess iter)
    (let ((next (f guess)))
      (display-all . (iter "  " guess))
      (newline)
      (if (close-enough guess next)
	  next
	  (try next (+ iter 1)))))
  (try init-guess 1))

;; without damping => 38 iterations
;; x^x = 1000 <=> x = (log 1000 / log x)
;; define f(x) = (log 1000 / log x) and find fixed point
(fixed-point
 (lambda (x) (/ (log 1000) (log x)))
 1.1)

;; with average damping => 14 iterations
;; x' = 0.5 * (x + (log 1000)/(log x))
(fixed-point
 (lambda (x) (* 0.5 (+ x (/ (log 1000) (log x)))))
 1.1)
