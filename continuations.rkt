#lang racket
; COSI 121B Structure and Interpretation of Computer Programs
; Problem Set 5: Continuations
; Irene Guo
; March 14, 2023

(define (I v) v)
(define (square n) (* n n))
(define (double n) (* 2 n))

; Question 1 Member?
(define (member? x L)
  (if (null? L)
      #f
      (if (equal? x (car L))
          #t
          (member? x (cdr L)))))
;(member? 5 '(1 3 5 7 9)) ;t
;(member? 5 '(0 2 4 6 8)) ;f
(define (Member? x L k)
  (if (null? L)
      (k #f)
      (if (equal? x (car L))
          (k #t)
          (Member? x (cdr L) (lambda (v) (k v))))))
;(Member? 5 '(1 3 5 7 9) I) ;t
;(Member? 5 '(0 2 4 6 8) I) ;f

; Question 2 Fastexp
(define (fastexp b e)
  (if (= e 0)
      1
      (if (even? e)
          (square (fastexp b (/ e 2)))
          (* b (fastexp b (- e 1))))))
;(fastexp 3 4) ;81
;(fastexp 2 17) ;131072
(define (Fastexp b e k)
  (if (= e 0)
      (k 1)
      (if (even? e)
          (Fastexp b (/ e 2) (lambda (v) (k (square v))))
          (Fastexp b (- e 1) (lambda (v) (k (* b v)))))))
;(Fastexp 3 4 I) ;81
;(Fastexp 2 17 I) ;131072

; Question 3 Fastmult
(define (fastmult m n)
  (if (= n 0)
      0
      (if (even? n)
          (double (fastmult m (/ n 2)))
          (+ m (fastmult m (- n 1))))))
;(fastmult 123 456) ;56088
(define (Fastmult m n k)
  (if (= n 0)
      (k 0)
      (if (even? n)
          (Fastmult m (/ n 2) (lambda (v) (k (double v))))
          (Fastmult m (- n 1) (lambda (v) (k (+ m v)))))))
;(Fastmult 123 456 I) ;56088

; Question 4 Map
(define (map f L)
  (if (null? L)
      '()
      (cons (f (car L)) (map f (cdr L)))))
;(map double '(11 12 13 14 15))
(define (Map f L k)
  (if (null? L)
      (k '())
      (Map f (cdr L) (lambda (v) (k (cons (f (car L)) v))))))
;(Map double '(11 12 13 14 15) I)

; Question 5 Filter
(define (filter pred L)
  (if (null? L)
      '()
      (if (pred (car L))
          (cons (car L) (filter pred (cdr L)))
          (filter pred (cdr L)))))
;(filter even? '(11 12 13 14 15))
(define (Filter pred L k)
  (if (null? L)
      (k '())
      (if (pred (car L))
          (Filter pred (cdr L) (lambda (v) (k (cons (car L) v))))
          (Filter pred (cdr L) k))))
;(Filter even? '(11 12 13 14 15) I)

; Question 6 Tack
(define (tack x L)
  (if (null? L)
      (cons x '())
      (cons (car L) (tack x (cdr L)))))
;(tack 7 '(3 1 4 1 5 9 2 6))
(define (Tack x L k)
  (if (null? L)
      (k (cons x '()))
      (Tack x (cdr L) (lambda (v) (k (cons (car L) v))))))
;(Tack 7 '(3 1 4 1 5 9 2 6) I)

; Question 7 Reverse
(define (reverse L)
  (if (null? L)
      '()
      (tack (car L) (reverse (cdr L)))))
;(reverse '(3 1 4 1 5 9 2 6))
(define (Reverse L k)
  (if (null? L)
      (k '())
      (Reverse (cdr L) (lambda (v) (Tack (car L) v k)))))
;(Reverse '(3 1 4 1 5 9 2 6) I)

; Question 8 Append
(define (append S T)
  (if (null? S)
      T
      (cons (car S) (append (cdr S) T))))
;(append '(a b c) '(d e))
(define (Append S T k)
  (if (null? S)
      (k T)
      (Append (cdr S) T (lambda (v) (k (cons (car S) v))))))
;(Append '(a b c) '(d e) I)

; Question 9 Fib
(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2))))) 
;(fib 10)
(define (Fib n k)
  (if (< n 2)
      (k n)
      (Fib (- n 1)
           (lambda (a)
             (Fib (- n 2)
                  (lambda (b) (k (+ a b))))))))
;(Fib 10 I)

; Question 10 Fringe
(define (fringe S)
  (if (null? S)
      '()
      (if (number? S)
          (list S)
          (append (fringe (car S)) (fringe (cdr S))))))
;(fringe '(((() ()) 3) (((1 4 5)) 9) (2 ((6 3)) 6) 8))
(define (Fringe S k)
  (if (null? S)
      (k '())
      (if (number? S)
          (k (list S))
          (Fringe (car S)
                  (lambda (a)
                    (Fringe (cdr S)
                            (lambda (b)
                              (Append a b k))))))))
;(Fringe '(((() ()) 3) (((1 4 5)) 9) (2 ((6 3)) 6) 8) I)

; Question 11 Tag
(define (tag x L)
  (if (null? L)
      '()
      (cons (cons x (car L)) (tag x (cdr L)))))
;(tag 21 '(2 7 1 8 2 8 5))
(define (Tag x L k)
  (if (null? L)
      (k '())
      (Tag x (cdr L) (lambda (v) (k (cons (cons x (car L)) v))))))
;(Tag 21 '(2 7 1 8 2 8 5) I)

; Question 12 Powerset
(define (powerset S)
  (if (null? S)
      '(())
      ((lambda (T) (append (tag (car S) T) T))
       (powerset (cdr S)))))
;(powerset '(A B C D))
(define (Powerset S k)
  (if (null? S)
      (k '(()))
      (Powerset (cdr S)
                (lambda (v) (Append (Tag (car S) v I) v k)))))
;(Powerset '(A B C D) I)

; Question 13 Cross
(define (cross S T)
  (if (null? S)
      '()
      (append (tag (car S) T)
              (cross (cdr S) T))))
;(cross '(A B C D) '(1 2 3))
(define (Cross S T k)
  (if (null? S)
      (k '())
      (Cross (cdr S) T
             (lambda (v) (Append (Tag (car S) T I) v k)))))
;(Cross '(A B C D) '(1 2 3) I)

; Question 14 Largers
(define (largers x L)
  (filter (lambda (n) (>= n x)) L))
;(largers 5 '(3 1 4 1 5 9 2 6 2 7 1 8 2 8))
(define (Largers x L k)
  (Filter (lambda (n) (>= n x)) L I))
;(Largers 5 '(3 1 4 1 5 9 2 6 2 7 1 8 2 8) I)

; Question 15 Smallers
(define (smallers x L)
  (filter (lambda (n) (< n x)) L))
;(smallers 5 '(3 1 4 1 5 9 2 6 2 7 1 8 2 8))
(define (Smallers x L k)
  (Filter (lambda (n) (< n x)) L I))
;(Smallers 5 '(3 1 4 1 5 9 2 6 2 7 1 8 2 8) I)

; Question 16 Quicksort
(define (quicksort F)
  (if (null? F)
      '()
      (append (quicksort (smallers (car F) (cdr F)))
              (cons (car F)
                    (quicksort (largers (car F) (cdr F)))))))
;(quicksort '(3 1 4 1 5 9 2 6 2 7 1 8 2 8))
(define (Quicksort F k)
  (if (null? F)
      (k '())
      (Quicksort (Smallers (car F) (cdr F) I)
                 (lambda (a)
                   (Quicksort (Largers (car F) (cdr F) I)
                              (lambda (b)
                                (Append a (cons (car F) b) k)))))))
;(Quicksort '(3 1 4 1 5 9 2 6 2 7 1 8 2 8) I)


