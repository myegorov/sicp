#lang sicp
(#%require racket/trace)

;; Ex. 1.9
;; iterative procedure: a and b suffice to describe the state
(define (iter+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

;; linear recursive procedure: defer evaluating outermost inc
;; until all operands have been expanded (= a 0);
;; the procedure is not tail recursive
(define (rec+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

;; Ex. 1.10
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

;; (A 1 10) => 2 * A (1 9) => 2 * 2 * A (1 8) ... => 2^10
;; (A 2 4) => (A 1 (A 2 3)) => (A 1 16) => 2^(2^(2^2))
;; (A 3 3) => (A 2 (A 3 2)) => (A 2 4)

(define (f n) (A 0 n)) ;; => f(n) = 2n
(define (g n) (A 1 n)) ;; => g(n) = 2^n
(define (h n) (A 2 n)) ;; => h(n) = 2^(2^(2^...)) n times total

;; naive Fibonacci
(define (fib-naive n)
  (cond ((= n 0) 0)
	((= n 1) 1)
	(else (+ (fib-naive (- n 1))
		 (fib-naive (- n 2))))))

;; iterative Fibonacci
;; 0 1 1 2 3
(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter curr prev count)
  (if (= count 0)
      prev 
      (fib-iter (+ curr prev) curr (dec count))))

;; count change
(define (count-change amount)
  (cc amount 5))

(define (cc amount num-denominations)
  (cond ((= num-denominations 0) 0)
	((= amount 0) 1)
	((< amount 0) 0)
        (else  (+ (cc amount (- num-denominations 1))
		  (cc (- amount
			 (max-value num-denominations))
		      num-denominations)))))

(define (max-value num-denominations)
  (cond ((= num-denominations 5) 50)
	((= num-denominations 4) 25)
	((= num-denominations 3) 10)
	((= num-denominations 2) 5)
	((= num-denominations 1) 1)))

;; Ex. 1.11
;; 0 1 2 4 11 ...
;; recursive procedure implementation
(define (ff n)
  (cond ((< n 3) n)
	(else (+ (ff (- n 1))
		 (* 2 (ff (- n 2)))
		 (* 3 (ff (- n 3)))))))
;; iterative implementation
(define (fff n)
  (fff-iter n 2 1 0))

(define (fff-iter n first second third)
  (define (helper-sum first second third)
    (+ first
       (* 2 second)
       (* 3 third)))
  (cond ((= n 0) third)
	(else (fff-iter (- n 1)
			(helper-sum first second third)
			first
			second))))
