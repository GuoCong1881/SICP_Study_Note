#lang racket
;Lecture 3 - Insertion Sort

;  1.1 insert an element into a sorted list
(define (insertSorted x L)
  (cond ((null? L)(list x))
        ((< x (car L))(cons x L))
        (else (cons (car L)(insertSorted x (cdr L))))))

;  e.g. with substitution model
(insertSorted 10 (list 1 3 5 7 9 11))
(cons 1(insertSorted 10 (list 3 5 7 9 11)))
(cons 1(cons 3 (insertSorted 10 (list 5 7 9 11))))
(cons 1(cons 3 (cons 5 (insertSorted 10 (list 7 9 11)))))
(cons 1(cons 3 (cons 5 (cons 7 (insertSorted 10 (list 9 11))))))
(cons 1(cons 3 (cons 5 (cons 7 (cons 9 (insertSorted 10 (list 11)))))))
(cons 1(cons 3 (cons 5 (cons 7 (cons 9 (cons 10 (list 11)))))))
(cons 1(cons 3 (cons 5 (cons 7 (cons 9 (list 10 11))))))
(cons 1(cons 3 (cons 5 (cons 7 (list 9 10 11)))))
(cons 1(cons 3 (cons 5 (list 7 9 10 11))))
(cons 1(cons 3 (list 5 7 9 10 11)))
(cons 1(list 3 5 7 9 10 11))
(list 1 3 5 7 9 10 11)


; 1.2 insertion sort
(define (insertionSort L)
  (if (null? L)
      '()
      (insertSorted (car L) (insertionSort (cdr L)))))

;  e.g. with substitution model
(insertionSort (list 3 5 2 8))
(insertSorted 3 (insertionSort (list 5 2 8)))
(insertSorted 3 (insertSorted 5 (insertionSort (list 2 8))))
(insertSorted 3 (insertSorted 5 (insertSorted 2 (insertionSort (list 8)))))
(insertSorted 3 (insertSorted 5 (insertSorted 2 (insertSorted 8 (insertionSort '())))))
(insertSorted 3 (insertSorted 5 (insertSorted 2 (insertSorted 8 '()))))
(insertSorted 3 (insertSorted 5 (insertSorted 2 (list 8))))
(insertSorted 3 (insertSorted 5 (list 2 8)))
(insertSorted 3 (list 2 5 8))
(list 2 3 5 8)


