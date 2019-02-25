#lang sicp
(#%require racket/trace)

;; Ex. 2.1
(define (gcd a b)
  (define (helper n d)
    (if (= d 0)
	n
	(helper d (remainder n d))))
  (if (> a b)
      (helper a b)
      (helper b a)))

;; constructor
(define (make-rat n d)
  (define (normalize-sign a b)
    (if (< (* a b) 0)
	(cons (- (abs a)) (abs b))
	(cons (abs a) (abs b))))
  (let ((common-factor (abs(gcd n d)))
	(signed (normalize-sign n d)))
    (cons (/ (car signed) common-factor)
	  (/ (cdr signed) common-factor))))

;; selectors
(define (numer r)
  (car r))
(define (denom r)
  (cdr r))

;; convenience funcs
(define (print-rat r)
  (display (numer r))
  (display "/")
  (display (denom r))
  (newline))

;; operations on rational nums
(define (add-rat r1 r2)
  (make-rat (+ (* (numer r1) (denom r2))
	       (* (numer r2) (denom r1)))
	    (* (denom r1) (denom r2))))

(define (sub-rat r1 r2)
  (make-rat (- (* (numer r1) (denom r2))
               (* (numer r2) (denom r1)))
            (* (denom r1) (denom r2))))

(define (mul-rat r1 r2)
  (make-rat (* (numer r1) (numer r2))
            (* (denom r1) (denom r2))))

(define (div-rat r1 r2)
  (make-rat (* (numer r1) (denom r2))
            (* (denom r1) (numer r2))))

(define (equal-rat? r1 r2)
  (= (* (numer r1) (denom r2))
     (* (numer r2) (denom r1))))

;; testing
(print-rat (add-rat (make-rat 1 3) (make-rat 1 3))) ;; 2/3
(print-rat (make-rat (- 2) (- 4))) ;; 1/2
(print-rat (make-rat 2 (- 4))) ;; -1/2
(print-rat (make-rat (- 2) 4)) ;; -1/2
(print-rat (make-rat 2 4)) ;; 1/2
