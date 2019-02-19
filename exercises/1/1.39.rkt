#lang sicp
(#%require racket/trace)

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


;; Ex. 1.39
;; Lambert's continued fraction representation of (tan x)
(define (square x) (* x x))
(define (tan-cf x k)
  (cont-frac-iter
   (lambda (iter) (if (= iter 1) x (- (square x))))
   (lambda (iter) (- (* 2 iter) 1))
   k))

(define PI 3.141592654)
(tan-cf (/ PI 4.0) 10) ;; ~ 1.0
