#lang racket
; Section 3.1 Assignment and Local state
; Section 3.1.3 The Costs of Introducing Assignment

; iterative factorial program in funcional programming
(define (factorial_f n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* product counter) (+ counter 1))))
  (iter 1 1))

; iterative factorial program in imperative programming
(define (factorial_i n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! product (* counter product))
                 (set! counter (+ counter 1))
                 (iter))))
    (iter)))


; careflly consider the relative orders of the assignment to make sure
; that each statement is using th ecorrect version of the variables
(define (factorial_iw n)
  (let ((product 1)
        (counter 1))
    (define (iter)
      (if (> counter n)
          product
          (begin (set! counter (+ counter 1))
                 (set! product (* counter product))
                 (iter))))
    (iter)))

(factorial_f 5)
(factorial_i 5)
(factorial_iw 5)