#lang sicp
(#%require racket/trace)

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (odd? x)
  (not (even? x)))

;; Ex. 1.28: Use Miller-Rabin test

(define (nontrivial-sqrt? x n)
  (and (= 1 (remainder (square x) n))
       (not (= x 1))
       (not (= x (- n 1)))))

;; compute (a ^ n) mod m
(define (expmod a n m)
  (cond ((= n 0) 1)
	((and (even? n)
	      (not (nontrivial-sqrt? (expmod a (/ n 2) m) m)))
	 (remainder (square (expmod a (/ n 2) m)) m))
	((odd? n) (remainder (* a (expmod a (- n 1) m))
			     m))
	(else 0))) ;; flag found nontrivial-sqrt

;; Carmichael numbers to test:
;; 561, 1105, 1729, 2465, 2821, 6601...
(define (exhaustive-miller-rabin-test n)
  (define (helper iter)
    ;; it suffices to test 1 <= a < n/2 to reveal
    ;; nontrivial sqrt 1 mod n if n composite
    (cond ((> iter (* (/ 1 2) n)) #t)  
	  ((= 1 (expmod iter (- n 1) n))
	   (helper (+ iter 1)))
	  (else #f)))
  (helper 1))
