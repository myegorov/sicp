#lang sicp
(#%require racket/trace)

(define (make-segment start end)
  (cons start end))
(define (start-segment s)
  (car s))
(define (end-segment s)
  (cdr s))

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

(define (midpoint-segment s)
  (define (average a b)
    (/ (+ a b) 2.0))
  (let ((start (start-segment s))
	(end (end-segment s)))
    (make-point
     (average (x-point start) (x-point end))
     (average (y-point start) (y-point end)))))

;; Ex. 2.3

;; representation 1
;; rectangles consist of bottom left and top right points (assuming oriented along major axes)
(define (make-rectangle bot-left-point top-right-point)
  (cons bot-left-point top-right-point))
(define (bot-left rec)
  (car rec))
(define (top-right rec)
  (cdr rec))

(define (width rec)
  (let ((p1 (bot-left rec))
	(p2 (top-right rec)))
    (abs (- (x-point p1) (x-point p2)))))

(define (height rec)
  (let ((p1 (bot-left rec))
	(p2 (top-right rec)))  
  (abs (- (y-point p1) (y-point p2)))))

(define (perimeter rec)
  (* 2 (+ (width rec) (height rec))))

(define (area rec)
  (* (width rec) (height rec)))

;; testing
(define rec (make-rectangle
	     (make-point 0 0)
	     (make-point 3 5)))


(perimeter rec)
(area rec)

;; representation 2
;; a rectangle could be defined by two points + rotation (radians)
(define (make-rectangle-rotated bot-left top-right rotation-ccw)
  (cons (cons bot-left top-right) rotation-ccw))

;; in local rectangle's coordinate frame
(define (bot-left-local rec)
  (car (car rec)))
(define (top-right-local rec)
  (cdr (car rec)))
(define (rotation rec)
  (cdr rec))

;; define rotation matrix
(define (R rotation)
  (cons (cons (cos rotation) (- sin (rotation)))
	(cons (sin rotation) (cos rotation))))

;; define vector (identical to segment)
(define make-vector make-segment)
;; TODO: define vector multiplication
;; TODO: define transformation to and selectors for global coordinates 

;; note that width, height, perimeter, and area procedures are same as before
