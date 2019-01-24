#lang sicp
(#%require racket/trace)

;; Ex. 1.7
(define (square x)
  (* x x))
(define (abs x)
  (if (< x 0)
      (- x)
      x))
(define (my-sqrt x)
  (define (average a b)
    (/ (+ a b) 2.0))
  (define (improve guess target)
    (average guess (/ target guess)))
  (define TOL 0.001)
  (define (good-enough? guess target)
    (if (< (abs (- target (square guess))) TOL)
	#t
	#f))
  (define (iter-sqrt x guess)
    (if (good-enough? guess x)
	guess
	(iter-sqrt x (improve guess x))))
  (iter-sqrt x 1.0))

;; (1) show TOL inadequate for very small numbers:
;; for example for any number < 0.001, the sqrt will be estimated
;; at approx (sqrt 0.001), i.e. in the limit the error is O(0.001)
;; which may be order of magnitudes error
(my-sqrt 0.000001) ; => 0.03125 instead of 0.001 (3000% diff)

;; (2) show the logic is inadequate for very large numbers:
;; (good-enough?) fails to make progress for (mysqrt 1e13)
;; 64-bit floats have under 16 decimal digits of precision, which is
;; more than the 16 digits required where (= TOL 0.001)
