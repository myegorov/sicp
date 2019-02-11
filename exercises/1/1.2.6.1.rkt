#lang sicp
(#%require racket/trace)

(define (square x)
  (* x x))

;; Ex. 1.21, 1.23
;; smallest divisor
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (define (test-smallest-divisor n test)
    (cond ((> (square test) n) n)
	  ((= (remainder n test) 0) test)
	  (else (test-smallest-divisor n
;;				       (next test)))))
  				       (+ test 1)))))
  (test-smallest-divisor n 2))

;; Ex. 1.22
;; naive prime check
(define (prime? x)
  (= x (smallest-divisor x)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time)
  (newline))

;; (timing for original version, Ex. 1.21)
;; ~1e3 => 5 (microseconds)
;; ~1e4 => 11
;; ~1e5 => 53
;; ~1e6 => 240
;; ~O(sqrt n)
(define (search-for-primes start end)
  (define (search-helper start end)
    (if (<= start end)
	(timed-prime-test start))
    (if (< start end)
	(search-helper (+ start 2) end)))
  (if (= (remainder start 2) 0)
      (search-helper (+ start 1) end)
      (search-helper start end)))
