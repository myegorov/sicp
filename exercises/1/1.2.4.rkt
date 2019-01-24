#lang sicp
(#%require racket/trace)

;; Ex 1.17
(define (double x)
  (+ x x))
(define (halve x)
  ;; precondition: (even? x) => #t
  (/ x 2))
(define (even? x)
  (= (remainder x 2) 0))

;; not yet tail recursive
(define (* x y)
  ;; precondition: (or (< x 0) (< y 0)) => #f
  (define (** b n tot)
    (cond ((= n 0) 0)
	  ((= n 1) tot)
	  ((even? n) (** b (halve n) (double tot)))
	  (else (+ b (** b (halve (- n 1)) (double tot))))))

  (** x y x))

;; Ex. 1.18
;; iterative implementation of Ex. 1.17
(define (mult-iter x y)
  ;; precondition: (or (< x 0) (< y 0)) => #f
  (define (** x y init)
    (cond ((or (= y 0) (= x 0)) 0)
	  ((= y 1) (+ x init))
	  ((even? y) (** (double x) (halve y) init))
	  (else (** (double x) (halve (- y 1)) x))))
 
  (** x y 0))
