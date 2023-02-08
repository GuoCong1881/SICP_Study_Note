#lang racket
; Section 2.2 Hierarchical Data adn the Closure Property 
; Section 2.2.2 Hierarchical Structures
; The elements of the sequence are the branches of the tree, and elements that are themselves sequences are subtrees

; 1.1 Review on length of a list
(define (length L)
  (if (null? L)
      0
      (+ 1 (length (cdr L)))))

; 1.2 count-leaves: count all leaves on the tree
(define (count-leaves T)
  (cond ((null? T) 0)
        ((not (pair? T)) 1)
        (else (+ (count-leaves (car T))
                 (count-leaves (cdr T))))))
      
; e.g. comparision
(define x (cons (list 1 2) (list 3 4)))
(length x)
(count-leaves x)
(length (list x x))
(count-leaves (list x x))

; 2. Mapping over trees

; 2.1 scale the leaves on a tree
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* factor tree))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))
(define myTree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
(scale-tree myTree 10)

; 2.2 another way of implementing: with map
(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* factor sub-tree)))
       tree))
(scale-tree-map myTree 20)


; Exercise 2.25
(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car (list (list 7))))
(define thisList (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr thisList))))))))))))

; Exercise 2.26
(define a (list 1 2 3))
(define b (list 4 5 6))
(append a b)
(cons a b)
(list a b)

; Exercise 2.18 reverse: a procedure that takes a list as argument and returns a list of the same elements in reverse order
(define (reverse L)
  (if (null? L)
      '()
      (append (reverse (cdr L)) (list (car L)))))
(reverse (list 1 3 5 7 9))

; Exercise 2.27 deep reverse
;    approach 1
(define (deep-reverse L)
  (cond ((null? L) '())
        ((number? (car L))
         (append (deep-reverse (cdr L)) (list (car L))))
        (else (append (deep-reverse (cdr L))
                      (list (deep-reverse (car L)))))))
                       
(define c (list (list 1 2) (list 3 4)))
(define d (list 1 2 (list 3 4) 5 (list 6 (list 7 8) 9) 10))
(deep-reverse c)
(deep-reverse d)
;   approach 2
(define (deep-reverse2 x)
  (if (pair? x)
      (append (deep-reverse2 (cdr x))
              (list (deep-reverse2 (car x))))
      x)) 
;      subsititution model
(deep-reverse2 c)
(append (deep-reverse2 (list(list 3 4)))
        (list (deep-reverse2 (list 1 2))))
(append (append (deep-reverse2 '())
                (list (deep-reverse2 (list 3 4))))
        (list (append (deep-reverse2 (list 2))
                      (list (deep-reverse2 1)))))
(append (append '()
                (list (append (deep-reverse2 (list 4))
                              (list (deep-reverse2 3)))))
        (list (append (append (deep-reverse2 '())
                              (list (deep-reverse2 2)))
                      (list (deep-reverse2 1)))))
(append (append '()
                (list (append (append '() (list 4))
                              (list 3))))
        (list (append (append '()
                              (list 2))
                      (list 1))))
(append (list (list 4 3))
        (list (list 2 1)))


; Exercise 2.28 fringe: a procedure that takes as argument a tree (represented as a list) and returns a list
; whose elements are all the leaves of the tree arranged in left-to-right order
(define (fringe T)
  (cond ((null? T) '())
        ((number? T) (list T))
        (else (append (fringe (car T)) (fringe (cdr T))))))  
        
     
(define e (list (list 1 2) (list 3 4)))
(fringe e)
(fringe (list e e))










