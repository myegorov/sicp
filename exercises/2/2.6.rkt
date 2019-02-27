#lang sicp
(#%require racket/trace)

;; Ex. 2.6: Church numerals
(define zero
  (lambda (f)
    (lambda (x) x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

;; same as (add-1 zero)
(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (+ c1 c2)
  (lambda (f)
    (lambda (x)
      ((c1 f) ((c2 f) x)))))
