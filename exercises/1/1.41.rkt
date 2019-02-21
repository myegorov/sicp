#lang sicp
(#%require racket/trace)

;; Ex. 1.41
(define (double f)
  (lambda (x)
    (f (f x))))
;((double inc) 2) ;; => 4

(((double (double double)) inc) 5) ;; (+ 16 5)
((double (double (double (double inc)))) 5) ;; (+ 16 5)
