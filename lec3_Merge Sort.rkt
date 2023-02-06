#lang racket
;Lecture 3 - Merge Sort

;1.1 odds
(define (odds L)
  (if (null? L)
      '()
      (cons (car L) (evens (cdr L)))))

;1.2 Evens
(define (evens L)
  (if (null? L)
      '()
      (odds (cdr L))))


;  e.g. with subsitution model
(odds (list 1 3 5 7 2 4 6))
(cons 1 (evens (list 3 5 7 2 4 6)))
(cons 1 (odds (list 5 7 2 4 6)))
(cons 1 (cons 5 (evens (list 7 2 4 6))))
(cons 1 (cons 5 (odds (list 2 4 6))))
(cons 1 (cons 5 (cons 2 (evens (list 4 6)))))
(cons 1 (cons 5 (cons 2 (odds (list 6)))))
(cons 1 (cons 5 (cons 2 (cons 6 (evens '())))))
(cons 1 (cons 5 (cons 2 (cons 6 '()))))
(list 1 5 2 6)

;1.3 merge two sorted lists
(define (merge L1 L2)
  (cond ((null? L1) L2)
        ((null? L2) L1)
        ((< (car L1) (car L2))(cons (car L1) (merge (cdr L1) L2)))
        (else (cons (car L2) (merge L1 (cdr L2))))))

(merge (list 1 3 5) (list 2 4 6))

;1.4 merge sort
(define (msort L)
  (if (< (length L) 2)
      L
      (merge (msort (odds L)) (msort (evens L)))))

;  e.g. with subsitution model
(msort (list 7 3 5 8 1 4))
(merge (msort (list 7 5 1))
       (msort (list 3 8 4)))
(merge (merge (msort (list 7 1))
              (msort (list 5)))
       (merge (msort (list 3 4))
              (msort (list 8))))
(merge (merge (merge (msort (list 7)) (msort (list 1)))
              (merge (msort (list 5)) (msort '())))
       (merge (merge (msort (list 3)) (msort (list 4)))
              (merge (msort (list 8)) (msort '()))))
(merge (merge (merge (list 7) (list 1))
              (merge (list 5) '()))
       (merge (merge (list 3) (list 4))
              (merge (list 8) '())))
(merge (merge (list 1 7)
              (list 5))
       (merge (list 3 4)
              (list 8)))
(merge (list 1 5 7)
       (list 3 4 8))
(list 1 3 4 5 7 8)

