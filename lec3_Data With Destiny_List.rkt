#lang racket
;1. List is syntactic sugar of cons
(define myList (list 10 20 30 40 50))
(cons 10 (cons 20 (cons 30 (cons 40 (cons 50 '())))))

;2. Recursion on list
;  2.1 Length
(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length(cdr lst)))))
(length myList)
;  2.2 Sum
(define (sum-list lst)
  (if (null? lst)
      0
      (+ (car lst) (sum-list (cdr lst)))))
(sum-list myList)
;  2.3 Append
(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1)
            (append (cdr list1) list2))))
(define otherList (list 60 70 80 90))
(append myList otherList)

;  2.4 Reverse
(define (reverse L)
  (if (null? L)
      '()
      (append (reverse (cdr L)) (list (car L)))))
(reverse myList)

;  2.5 Append to the end (Tack)
(define (tack x L)
  (if (null? L)
      (list x)
      (cons (car L) (tack x (cdr L)))))
(tack 60 myList)
(append myList (list 60))

; 2.6 Reverse 2.0 with Tack
(define (reverse2 L)
  (if (null? L)
      '()
      (tack (car L) (reverse2 (cdr L)))))
(reverse2 myList)
      

;3. List-processing functions
;  3.1 map
(define (map fn L)
  (if (null? L)
      '()
      (cons (fn (car L))
            (map fn (cdr L)))))
(define (square x) (* x x))
(map square myList)

;  3.2 filter
(define (filter pred L)
  (if (null? L)
      '()
      (if (pred (car L))
          (cons (car L)
                (filter pred (cdr L)))
          (filter pred (cdr L)))))
(define (even? x) (= 0 (remainder x 2)))
(filter even? (list 0 1 2 3 4 5))

;  3.3 enumerate
(define (integers from to)
  (if (> from to)
      '()
      (cons from (integers (+ from 1) to))))
(integers 10 20)

; 3.4 composition: by composing these procedures together, we begin to get a new idiom for writing programs
(map square (integers 10 30))
(filter even? (map square (integers 10 30)))
(sum-list (filter even? (map square (integers 10 30))))

; 3.5 flatmap (fn is a list-processing procedure)
(define (flatmap fn L)
  (if (null? L)
       '()
       (append (fn (car L))
               (flatmap fn (cdr L)))))
(map (lambda(x) (integers x (+ x 5))) (list 10 20 30))
(flatmap (lambda (x) (integers x (+ x 5))) (list 10 20 30))

;4. Many of these procedures have a common pattern that can be abstracted
(define (accumulate combiner null-value term L)
  (if (null? L)
      null-value
      (combiner (term (car L))
                (accumulate combiner null-value term (cdr L)))))

;  4.1 append with accumulate abstraction
(define (appendAccu list1 list2)
  (accumulate cons list2 (lambda(x) x) list1))
(appendAccu (list 1 2 3) (list 4 5 6))

;  4.2 map with accumulate abstraction
(define (mapAccu fn L)
  (accumulate cons '() fn L))
(mapAccu square (list 1 2 3 4 5 6))

; 4.3 reverse with accumulate abstraction
(define (reverseAccu L)
  (accumulate tack '() (lambda(x) x) L))
(reverseAccu (list 1 2 3 4))

; 4.4 length with accumulate abstraction
(define (lengthAccu L)
  (accumulate + 0 (lambda(x) 1) L))
(lengthAccu (list 1 2 3 4 5))

; 4.5 filter with accumulate abstraction !!!!????
(define (filterAccu pred L)
  (accumulate (lambda (x y) (if (pred x) cons y))
              '()
              (lambda (x) x) 
              L))
(filterAccu even? (list 1 2 3 4 5 6 7 8))
 
