#lang racket
;lec3_Data WIth Destiny - permutation

;1. Permutation
;  1.1 Step 1: insert x to every interval of L (return a list of list)
(define (insert x L)
  (if (null? L)
      (list (list x))
      (cons (cons x L)
            (map (lambda (M) (cons (car L) M))
                 (insert x (cdr L))))))

;  (helper function: flatmap)
(define (flatmap fn L)
  (if (null? L)
       '()
       (append (fn (car L))
               (flatmap fn (cdr L)))))

;  1.2 Step 2: permutation
(define (perms L)
  (if (null? L)
      (list '())
      (flatmap (lambda (M) (insert (car L) M))
               (perms (cdr L)))))

;  e.g. with substitution model
(perms (list 1 2 3))

(flatmap (lambda (M) (insert 1 M))
         (perms (list 2 3)))

(flatmap (lambda (M) (insert 1 M))
         (flatmap (lambda (M) (insert (car (list 2 3)) M))
                  (perms (cdr (list 2 3)))))

(flatmap (lambda (M) (insert 1 M))
         (flatmap (lambda (M) (insert 2 M))
                  (flatmap (lambda (M) (insert (car (list 3)) M))
                           (perms (cdr (list 3))))))

(flatmap (lambda (M) (insert 1 M))
         (flatmap (lambda (M) (insert 2 M))
                  (flatmap (lambda (M) (insert 3 M))
                           (list '()))))

(flatmap (lambda (M) (insert 1 M))
         (flatmap (lambda (M) (insert 2 M))
                  (list (list 3))))

(flatmap (lambda (M) (insert 1 M))
         (list (list 2 3) (list 3 2)))


(append ((lambda (M) (insert 1 M))
         (car (list (list 2 3) (list 3 2))))
        (flatmap (lambda (M) (insert 1 M))
                 (cdr (list (list 2 3) (list 3 2)))))

(append ((lambda (M) (insert 1 M)) (list 2 3))
        (flatmap (lambda (M) (insert 1 M))
                 (list (list 3 2))))

(append ((lambda (M) (insert 1 M)) (list 2 3))
        (append ((lambda (M) (insert 1 M)) (car (list (list 3 2))))
                (flatmap (lambda (M) (insert 1 M)) (cdr (list (list 3 2))))))

(append ((lambda (M) (insert 1 M)) (list 2 3))
        (append ((lambda (M) (insert 1 M)) (list 3 2))
                (flatmap (lambda (M) (insert 1 M)) '())))

(append ((lambda (M) (insert 1 M)) (list 2 3))
        (append ((lambda (M) (insert 1 M)) (list 3 2))
                '()))

(append ((lambda (M) (insert 1 M)) (list 2 3))
        ((lambda (M) (insert 1 M)) (list 3 2)))

(append (list (list 1 2 3)(list 2 1 3)(list 2 3 1))
        (list (list 1 3 2)(list 3 1 2)(list 3 2 1)))

                                 