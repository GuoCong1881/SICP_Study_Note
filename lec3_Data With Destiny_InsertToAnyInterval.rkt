#lang racket
;lec3_Data WIth Destiny - insert

;1. Permutation
;  1.1 Step 1: insert x to every interval of L (return a list of list)
(define (insert x L)
  (if (null? L)
      (list (list x))
      (cons (cons x L)
            (map (lambda (M) (cons (car L) M))
                 (insert x (cdr L))))))

(insert 1 '())
(cons 5 (list (list 1)))

;  e.g. with substitution model
(insert 1 (list 2 3 4 5))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (insert 1 (list 3 4 5))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
            (map (lambda (M) (cons 3 M))
                 (insert 1 (list 4 5))))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
            (map (lambda (M) (cons 3 M))
                 (cons (list 1 4 5)
                       (map (lambda (M) (cons 4 M))
                            (insert 1 (list 5))))))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
            (map (lambda (M) (cons 3 M))
                 (cons (list 1 4 5)
                       (map (lambda (M) (cons 4 M))
                             (cons (list 1 5)
                                   (map (lambda (M) (cons 5 M))
                                        (insert 1 '())))))))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
            (map (lambda (M) (cons 3 M))
                 (cons (list 1 4 5)
                       (map (lambda (M) (cons 4 M))
                             (cons (list 1 5)
                                   (map (lambda (M) (cons 5 M))
                                        (list (list 1))))))))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
            (map (lambda (M) (cons 3 M))
                 (cons (list 1 4 5)
                       (map (lambda (M) (cons 4 M))
                            (list (list 1 5) (list 5 1))))))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
            (map (lambda (M) (cons 3 M))
                 (cons (list 1 4 5)
                       (list (list 4 1 5)(list 4 5 1)))))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (cons  (list 1 3 4 5)
                  (list (list 3 1 4 5) (list 3 4 1 5)(list 3 4 5 1)))))

(cons (list 1 2 3 4 5)
      (map (lambda (M) (cons 2 M))
           (list (list 1 3 4 5)(list 3 1 4 5) (list 3 4 1 5)(list 3 4 5 1))))

(cons (list 1 2 3 4 5)
      (list (list 2 1 3 4 5)(list 2 3 1 4 5) (list 2 3 4 1 5)(list 2 3 4 5 1)))

(list (list 1 2 3 4 5)(list 2 1 3 4 5)(list 2 3 1 4 5) (list 2 3 4 1 5)(list 2 3 4 5 1))

