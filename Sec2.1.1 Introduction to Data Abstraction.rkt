#lang racket
;2.1.1 Example: Arithmetic Operations for Rational Numbers

;data abstraction of rational numbers - constructor and selector
(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

;print
(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;arithmetic operations:
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

;test case: 
(define x (make-rat 1 3))
(define y (make-rat 1 2))
(print-rat (add-rat x y))
(print-rat (sub-rat x y))
(print-rat (mul-rat x y))
(print-rat (div-rat x y))

  