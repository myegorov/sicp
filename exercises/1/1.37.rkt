#lang sicp
(#%require racket/trace)

;; Ex. 1.37
;; recursive implementation
(define (cont-frac-rec n d k)
  (define (helper iter)
    (let ((N (n iter))
	  (D (d iter)))
      (if (= iter k)
	  (/ N D)
	  (/ N (+ D (helper (+ iter 1)))))))
  (helper 1))


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

;; calculate phi as 1 + 1/(1 + 1/...)
(define phi
  (+ 1
     (cont-frac-iter
      (lambda (x) 1.0)
      (lambda (x) 1.0)
      10)))

(define PHI
  (/ (+ 1 (sqrt 5)) 2.0))
