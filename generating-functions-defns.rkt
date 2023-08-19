#lang racket

; Stream preliminaries

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b) (cons a (delay b)))))

(define head car)

(define (tail s) (force (cdr s)))

(define stream-car car)

(define stream-cdr tail)

(define the-empty-stream (delay '()))

(define (empty-stream? s)
 (and (not (pair? s))
      (null? (force s))))

(define (map-stream fn S)
  (cons-stream (fn (head S))
              (map-stream fn (tail S))))

(define (filter-stream pred S)
  (if (pred (head S))
      (cons-stream (head S) (filter-stream pred (tail S)))
      (filter-stream pred (tail S))))

(define (1+ x) (+ 1 x))

(define (take n G)
  (if (= n 0)
      '()
      (cons (head G) (take (- n 1) (tail G)))))

(define (drop n G)
  (if (= n 0)
      G
      (drop (- n 1) (tail G))))

(define zeros (cons-stream 0 zeros))

(define (ints n) (cons-stream n (ints (+ n 1))))

(define integers (ints 0))


; Display power series

(define (show n p)
  (define (showit i p)
    (if (> i n)
        '()
        (cons (if (= i 0)
                  (head p)
                  (list '* (head p) 'z^ i))
              (showit (+ i 1) (tail p)))))
  (cons '+ (showit 0 p)))



; Problem 0
(define (series L)
  (if (null? L)
      zeros
      (cons-stream (head L) (series (tail L)))))

; Problem 1 (has been recoded in Problem 2)
;(define (sum S1 S2)
;  (cons-stream (+ (head S1) (head S2))
;               (sum (tail S1) (tail S2))))

;(define (star S1 S2)
;  (cons-stream (* (head S1) (head S2))
;               (star (tail S1) (tail S2))))

; Problem 2
(define (zip-streams S1 S2)
  (cons-stream (cons (head S1) (head S2))
               (zip-streams (tail S1) (tail S2))))

(define (sum S1 S2) (map-stream (lambda (P) (+ (car P) (cdr P))) (zip-streams S1 S2)))
(define (star S1 S2) (map-stream (lambda (P) (* (car P) (cdr P))) (zip-streams S1 S2)))

; Problem 3
(define (scale c S)
  (map-stream (lambda (x) (* c x)) S))

; Problem 4
(define (prod-z S) (cons-stream 0 S))

; Problem 5
(define (deriv S) (star (ints 1) (tail S)))

; Problem 6
(define (prod G H)
  (cons-stream (* (head G) (head H))
               (sum (sum (scale (head H) (tail G))
                         (scale (head G) (tail H)))
                    (prod-z (prod (tail G) (tail H))))))

; Problem 7
;(define (divide G H)
;  (if (= 0 (head G))
;      (cons-stream 0
;                   (divide (tail G) H))
;      (cons-stream (/ (head G) (head H))
;                   (divide (tail (sum (scale -1
;                                       (prod (series (list (/ (head G) (head H))))
;                                             H))
;                                G)) H))))

(define (divide G H)
  (let ((div (/ (head G) (head H))))
    (cons-stream
     div
     (divide (sum (tail G) (scale (- div) (tail H))) H))))

(define (reciprocal S)
  (divide (series '(1)) S))


; Problem 8
(define (coeff n S) (head (drop n S)))

; Problem 9
(define (expt S n)
  (cond ((= n 0) (series '(1)))
        ((odd? n) (prod S (expt S (- n 1))))
        (else (expt (prod S S) (/ n 2)))))


(define (pascal n) (take (+ 1 n) (expt (series '(1 1)) n)))

(define (binomial n k)
  (coeff k (expt (series '(1 1)) n)))

; Problem 10
(define (hat G) (prod G (reciprocal (series '(1 -1)))))


; For problem 11

(define golden-mean (/ (+ 1 (sqrt 5)) 2))

(define F (scale (/ 1 (sqrt 5))
                 (sum (reciprocal (series (list 1 (- golden-mean))))
                      (scale -1 (reciprocal (series (list 1 (- golden-mean 1))))))))
  
