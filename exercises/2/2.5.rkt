#lang sicp
(#%require racket/trace)

;; Ex. 2.5
;; assuming n >= 0
(define (pow base n)
  (if (= n 0)
      1
      (* base (pow base (- n 1)))))

(define (cons a b)
  (* (pow 2 a) (pow 3 b)))

(define (factor-out pair base)
  (define (iter acc res)
    (let ((rem (remainder acc base))
	  (next (/ acc base)))
      (if (= rem 0)
	  (iter next (+ res 1))
	  res)))
  (iter pair 0))
  
(define (car pair)
  (factor-out pair 2))

(define (cdr pair)
  (factor-out pair 3))
