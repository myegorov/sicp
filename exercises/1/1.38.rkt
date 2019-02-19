#lang sicp
(#%require racket/trace)

;; Ex. 1.38
;; iterative implementation
(define (cont-frac-iter n d k)
  (define (helper iter acc)
    (let ((N (n iter))
	  (D (d iter)))
      (if (= iter 0)
	  acc
	  (helper (- iter 1)
		  (/ N (+ D acc))))))
  (helper k 0.0))


(define (n-i x) 1.0)
(define (d-i x)
  (if (= (remainder x 3) 2)
      (* 2 (/ (+ 1 x) 3.0))
      1))

;; Euler's expansion: e = 2 + (cont-frac n-i d-i k)
;; with 10 iterations
(define e
  (+ 2  (cont-frac-iter n-i d-i 10)))
