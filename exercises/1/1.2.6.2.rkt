#lang sicp
(#%require racket/trace)

(define (square x)
  (* x x))

(define (even? x)
  (= (remainder x 2) 0))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define ITER 10)

(define (start-prime-test n start-time)
  (if (probably-prime? n ITER)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

;; Ex. 1.24: Use Fermat test

;; compute (a ^ n) mod m
(define (expmod a n m)
  (cond ((= n 0) 1)
	((even? n)
	 (remainder (square (expmod a (/ n 2) m)) m))
	(else (remainder (* a
			    (expmod a (- n 1) m))
			 m))))

(define (random-int-less-than n)
  (+ (random (- n 1)) 1))

(define (prime? n)
  (define (try-it a)
    (= a (expmod a n n)))
  (try-it (random-int-less-than n)))

(define (probably-prime? n times)
  (cond ((= times 0) #t)
	((prime? n) (probably-prime? n (- times 1)))
	(else #f)))

;; we expect that testing 1e6 should take approx.
;; 2x as long as testing 1e3 for primality
(timed-prime-test 1009)
(timed-prime-test 1000003)

;; Ex. 1.25
;;
;; (define (expmod base exp m)
;;   (remainder (fast-expt base exp) m))
;;
;; (define (fast-expt b n)
;;   (cond ((= n 0) 
;;          1)
;;         ((even? n) 
;;          (square (fast-expt b (/ n 2))))
;;         (else 
;;          (* b (fast-expt b (- n 1))))))
;;
;; While this (expmod) definition is technically correct,
;; it requires calculating arbitrarily large exponents before
;; applying the (remainder) operation.
;; By contrast, the definition of (expmod) in Ex. 1.24 implies
;; that you would apply the (remainder) to at most exp^2 and the
;; product of (remainder) ops will likewise never exceed the
;; exp.

;; Ex. 1.27
;; Carmichael numbers to test:
;; 561, 1105, 1729, 2465, 2821, 6601...
(define (exhaustive-fermat-test n)
  (define (helper iter)
    (cond ((= iter 1) #t)
	  ((= iter (expmod iter n n))
	   (helper (- iter 1)))
	  (else #f)))
  (helper (- n 1)))
