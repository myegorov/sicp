#lang sicp
(#%require racket/trace)

(define (sum f start next end)
  (if (> start end)
      0
      (+ (f start)
	 (sum f (next start) next end))))

;; approximation of PI
(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(define (PI)
  (* 8 (pi-sum 1 100000)))

;; naive \int_{a}^{b}
(define (integral f a b dx)
  (define (next x)
    (+ x dx))
  (* dx
     (sum f (+ a (/ dx 2.0)) next b)))

(define (cube x) (* x x x))

(integral cube 0 1 0.01) ;; => ~0.25

;; Ex. 1.29: Simpson's rule
;; let u = a + k * h, a <= u <= b
;;     g(u) = K * f(u)
;;     next(u) = u + h
;; Simpson's rule yields exact result for polynomial of degree 3 or less
(define (even? x) (= (remainder x 2) 0))
(define (odd? x) (not (even? x)))
(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (next u) (+ u h))
  (define (K u)
    (cond ((or (= u a) (= u b)) 1)
	  ((odd? (/ (- u a) h)) 4)
	  (else 2)))
  (define (g u)
    (* (K u) (f u)))
  (* (/ h 3.0) (sum-iter g a next b)))

;; Ex. 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
	(iter (term (next a)) (+ result (term a)))))
  (iter a 0))
