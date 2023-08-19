#lang R5RS

; This procedure, not to be used, marks where in the code you have to fill stuff in...

(define ***fill-in-your-code-here*** '())

;;;; Beginning...

(define (ints from to)
  (if (> from to)
      '()
      (cons from (ints (+ 1 from) to))))


; a tail recursive reverse

(define (reverse L)
  (rev L '()))

(define (rev L S)
  (if (null? L)
      S
      (rev (cdr L) (cons (car L) S))))


;;;; Exercise 3 melding two trees

(define (meld a b)
  (if (and (number? a) (number? b))
      (if (> a b)
          (list a b)
          (list b a))
      (if (> (car a) (car b))
          (cons (car a) (cons b (cdr a)))
          (cons (car b) (cons a (cdr b))))))


;;;; Exercise 4  evenmeld

(define (evenmeld L)
  (if (null? L)
      '()
      (cons (meld (car L) (cadr L))
            (evenmeld (cddr L)))))


;;;; Exercise 5 trees

(define (trees L)
  (let ((length (len L)))
    (if (< length 2)
        L
        (if (even? length)
            (cons '() (trees (evenmeld L)))
            (cons (car L) (trees (evenmeld (cdr L))))))))

(define (len L)
  (define (iter remaining-L res)
    (cond ((number? remaining-L) 1)
          ((null? remaining-L) res)
          (else (iter (cdr remaining-L) (+ 1 res)))))
  (iter L 0))

(define (even? x) (= 0 (remainder x 2)))

(define (queue L)  (reverse (trees L)))


;;;; binary numbers

(define (binary n)
  (if (= n 0)
      (list 0)
      (bin n)))

(define (bin n)
  (if (= n 0)
      '()
      (if (even? n)
          (cons 0 (bin (/ n 2)))
          (cons 1 (bin (/ (- n 1) 2))))))

(define (decimal bs)
  (if (null? bs) 0 (+ (car bs) (* 2 (decimal (cdr bs))))))


;;;; Exercise 6 increment

(define (increment B)
  (define (iter remaining-B carry res)
    (cond ((null? remaining-B)
           (if (= carry 1)
               (append res (list carry))
               res))
          ((= (+ carry (car remaining-B)) 2) (iter (cdr remaining-B) 1 (append res (list 0))))
          ((= (+ carry (car remaining-B)) 1) (iter (cdr remaining-B) 0 (append res (list 1))))
          (else (iter (cdr remaining-B) 0 (append res (list 0))))))
  (iter B 1 '()))


;;;; Exercise 7  add

(define (plus a b)
  (if (< (length a) (length b))
      (add a b 0)
      (add b a 0)))

(define (add S L c)
  (if (= c 0)
      (if (null? S)
          L
          (if (= (car S) 0)
              (append (list (car L)) (add (cdr S) (cdr L) 0))
              ; (car S)= 1
              (if (= (car L) 0)
                  (append (list 1) (add (cdr S) (cdr L) 0))
                  ; (cdr L)= 1
                  (append (list 0) (add (cdr S) (cdr L) 1)))))
      ; c= 1
      (if (null? S)
          (increment L)
          (if (= (car S) 0)
              (if (= (car L) 0)
                  (append (list 1) (add (cdr S) (cdr L) 0))
                  (append (list 0) (add (cdr S) (cdr L) 1)))
              ; (car S)= 1
              (if (= (car L) 0)
                  (append (list 0) (add (cdr S) (cdr L) 1))
                  ; (car L)= 1
                  (append (list 1) (add (cdr S) (cdr L) 1)))))))

(define (check a b)
  (let ((as (binary a))
        (bs (binary b)))
    (let ((cs (plus as bs)))
      (write (list a '+ b '= (+ a b))) (newline)
      (write (list 'as '= as)) (newline)
      (write (list 'bs '= bs)) (newline)
      (write (list 'cs '= cs '=_10 (decimal cs))) (newline)
      cs)))


;;;; Exercise 8  max-queue

(define (max-queue Q)
  (define (iter remaining-Q max)
    (if (null? remaining-Q)
        max
        (if (null? (car remaining-Q))
            (iter (cdr remaining-Q) max)
            (if (number? (car remaining-Q))
                (if (> (car remaining-Q) max)
                    (car remaining-Q)
                    max)
                (if (> (car (car remaining-Q)) max)
                    (iter (cdr remaining-Q) (car (car remaining-Q)))
                    (iter (cdr remaining-Q) max))))))
  (iter Q -1))


;;;; Exercise 9  insert

(define (insert x Q)
  (reverse (insert-iter x (reverse Q))))

(define (insert-iter carry RQ)
  (cond ((null? RQ) (list carry))
        ; both (car RQ) and carry are null
        ((and (null? (car RQ)) (null? carry)) RQ)
        ; both (car RQ) and carry are not null
        ((and (not (null? (car RQ))) (not (null? carry)))
         (cons '() (insert-iter (meld (car RQ) carry) (cdr RQ))))
        ; carry is null, (car RQ) is not null
        ((null? carry) RQ)
        ; carry is not null, (car RQ) is null
        (else (cons carry (cdr RQ)))))


;;;; Exercise 10 find

(define (find v Q)
  (cond ((null? Q) '())
        ((null? (car Q)) (find v (cdr Q)))
        ((number? (car Q))
         (if (= v (car Q))
             v
             '()))
        (else (if (= v (car (car Q)))
                  (car Q)
                  (find v (cdr Q))))))


;;;; Exercise 11 remove

(define (remove v Q)
  (let ((res (remove-iter v Q)))
    (if (and (pair? res) (null? (car res)))
        (cdr res)
        res)))
  
(define (remove-iter v Q)
  (cond ((null? Q) '())
        ((number? (car Q))
         (if (= v (car Q))
             '(())
             Q)) 
        ((null? (car Q)) (cons '() (remove-iter v (cdr Q))))
        (else (if (= v (car (car Q)))
                  (cons '() (cdr Q))
                  (cons (car Q) (remove-iter v (cdr Q)))))))


;;;; Exercise 12 merge

(define (merge Q1 Q2)
  (reverse (merge-tree (reverse Q1) (reverse Q2) '())))

(define (merge-tree RQ1 RQ2 c)
  (if (null?  c)
      (if (null? RQ1)
          RQ2
          (if (null? RQ2)
              RQ1
              (if (null? (car RQ1))
                  (cons (car RQ2) (merge-tree (cdr RQ1) (cdr RQ2) '()))
                  (if (null? (car RQ2))
                      (cons (car RQ1) (merge-tree (cdr RQ1) (cdr RQ2) '()))
                      (cons '() (merge-tree (cdr RQ1) (cdr RQ2) (meld (car RQ1) (car RQ2))))))))
      (if (null? RQ1)
          (merge-tree (list c) RQ2 '())
          (if (null? RQ2)
              (merge-tree (list c) RQ1 '())
              (if (null? (car RQ1))
                  (if (null? (car RQ2))
                      (cons c (merge-tree (cdr RQ1) (cdr RQ2) '()))
                      (cons '() (merge-tree (cdr RQ1) (cdr RQ2) (meld c (car RQ2)))))
                  (if (null? (car RQ2))
                      (cons '() (merge-tree (cdr RQ1) (cdr RQ2) (meld c (car RQ1))))
                      (cons c (merge-tree (cdr RQ1) (cdr RQ2) (meld (car RQ1) (car RQ2))))))))))


;;;; Exercise 13 remove-max

(define (remove-max Q)
  (let ((max (max-queue Q)))
    (let ((max-tree (find max Q)))
      (if (number? max-tree)
          (merge (remove max Q) '())
          (merge (remove max Q) (cdr max-tree)))))) 

(define (test Q)
   (write Q)
   (newline)
   (if (null? Q)
       '()
       (test (remove-max Q))))

(define (make-queue) '())

