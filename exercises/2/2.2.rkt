#lang sicp
(#%require racket/trace)

;; Ex. 2.2
(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

;; testing
(define l (make-segment p1 p2))

(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

(define (print-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (newline))

;; testing
(define origin (make-point 0 0))
(define p1 (make-point 1 0))
(define p2 (make-point 0 1))
(print-point origin) ; (0,0)


(define (midpoint-segment s)
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((start (start-segment s))
	(end (end-segment s)))
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))

;; testing
(print-point (midpoint-segment l)) ; (0.5,0.5)
