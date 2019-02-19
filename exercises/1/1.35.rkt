#lang sicp
(#%require racket/trace)

;; Ex. 1.35
(define (fixed-point f init-guess)
  (define tolerance 0.00001)
  (define (close-enough a b)
    (< (abs (- a b)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough guess next)
	  next
	  (try next))))
  (try init-guess))

;; f(x) = 1 + 1/x
;; given phi^2 = phi + 1
;; => f(phi) = 1 + 1/phi = (phi + 1)/phi = (phi^2)/phi = phi

;; golden ratio ~ 1.618
;; (define phi
;;   (/ (+ 1 (sqrt 5)) 2))

;; use fixed-point to find phi
(fixed-point
  (lambda (x) (+ 1.0 (/ 1.0 x)))
  1.0) ;; => 1.618
