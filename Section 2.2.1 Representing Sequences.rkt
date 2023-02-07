#lang racket
; Section 2.2 Hierarchical Data adn the Closure Property 
; Section 2.2.1 Representing Sequences


;1 list operation

;1.1 list-ref: return the nth item of the list
(define (list-ref L n)
  (if (= 0 n)
      (car L)
      (list-ref (cdr L) (- n 1))))
(list-ref (list 1 3 5 7 9) 3)


;1.2 length: return the length of the list
;  1.2.1 recursion style
(define (length-r L)
  (if (null? L)
      0
      (+ 1 (length-r (cdr L)))))
(length-r (list 2 4 1 6 5))

;  1.2.2 iterative style
(define (length-i L)
  (define (length-iter items count)
    (if (null? items)
        count
        (length-iter (cdr items) (+ 1 count))))
  (length-iter L 0))
(length-i (list 2 4 1 6 5))

;1.3 append
(define (append L1 L2)
  (if (null? L1)
      L2
      (cons (car L1)
            (append (cdr L1) L2))))
(append (list 1 2 3) (list 4 5))


;2 Mapping over lists

;2.1 scale-list: scales each number in a list by a given factor
(define (scale-list L s)
  (if (null? L)
      '()
      (cons (* s (car L))
            (scale-list (cdr L) s))))
(scale-list (list 1 3 5) 2)

;2.2 map: takes as arguments a procedure of one argument and a list, returns a list of the results produced by applying the procedure to each element in the list
(define (map fn L)
  (if (null? L)
      '()
      (cons (fn (car L))
            (map fn (cdr L)))))
(map (lambda(x) (* x x)) (list 1 3 5))


;2.3 revisit scale-list
(define (scale-list-gen L s)
  (map (lambda(x) (* s x)) L))
(scale-list-gen (list 1 3 5) 2)

;Exercise 2.21
(define (square-list items)
  (if (null? items)
      '()
      (cons (* (car items) (car items)) (square-list (cdr items)))))
(square-list (list 1 3 5))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))
(square-list2 (list 2 3 4))

;Exercise 2.22 (with bug - return the reverse of answer)
(define (iter things answer)
  (if (null? things)
      answer
      (iter (cdr things)
            (cons (* (car things) (car things)) answer))))
(define (square-list3 items) (iter items '()))

;e.g. with substitution model
(square-list3 (list 2 3 4))
(iter (list 3 4) (list 4))
(iter (list 4) (list 9 4))

;try to debug: change the sequence of cons (but still fails)
(define (iter2 things answer)
  (if (null? things)
      answer
      (iter2 (cdr things)
            (cons answer (* (car things) (car things))))))
(define (square-list4 items) (iter2 items '()))
;e.g. with substitution model
(square-list4 (list 2 3 4))
(iter2 (list 3 4) (cons '() 4))




    