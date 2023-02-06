#lang racket
;Lecture 3 - Binary Trees


;1.1 Constructor
(define (leaf x) x)
(define (tree L R) (list L R))

;1.2 Destructor
(define (leafval L) L)
(define left car)
(define right cadr)

;1.3 Discriminator
(define (leaf? T) (not (pair? T)))
(define nonleaf? pair?)

;  e.g.
(leaf 3)

(define myTree (tree (leaf 3) (tree (leaf 10) (leaf 40))))
myTree


;1.4 leaves
(define (leaves T)
  (if (leaf? T)
      (list (leafval T))
      (append (leaves (left T))(leaves (right T)))))

;  e.g. with substitution model
(leaves (tree (leaf 3) (tree (leaf 10) (leaf 40))))
(append (leaves (left (tree (leaf 3) (tree (leaf 10) (leaf 40)))))
        (leaves (right (tree (leaf 3) (tree (leaf 10) (leaf 40))))))
(append (leaves (leaf 3))
        (leaves (tree (leaf 10) (leaf 40))))
(append (list 3)
        (append (leaves (leaf 10))(leaves (leaf 40))))
(append (list 3)
        (append (list 10)(list 40)))
(append (list 3)
        (list 10 40))
(list 3 10 40)





