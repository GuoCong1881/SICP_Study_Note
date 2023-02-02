#lang racket
;reading: Sec2.1.1

;1. Scheme's mechanism for making pairs
(define x (cons 10 20))
x
(car x)
(cdr x)
;Box and pointer diagrams
(define y (cons 10 (cons 20 30)))
(define z (cons (cons 10 20) 30))
y
z

;2. Making your own cons (consxy) without a computational deity
(define (consxy x y) (lambda (m) (if m x y)))
(define (carx z) (z #t))
(define (cdry z) (z #f))

;e.g. with substitution model
(carx (consxy 10 20))
(carx (lambda (m) (if m 10 20)))
((lambda (m) (if m 10 20)) #t)
(if #t 10 20)
10

;2.1 second way of making own cons (consxy2) - just give a name to the lambda procedure and return the same procedure
(define (consxy2 x y)
  (define (cell m)
    (if m x y))
  cell)
(define (carx2 z) (z #t))
(define (cdry2 z) (z #f))
(carx2 (consxy2 30 40))
30

;2.2 third way of making own cons (consxy3)
(define (consxy3 x y) (lambda (m) (m x y)))
(define (carx3 z) (z (lambda (x y) x)))
(define (cdry3 z) (z (lambda (x y) y)))

;e.g. with substitution model
(carx3 (consxy3 50 60))
(carx3 (lambda (m) (m 50 60)))
((lambda(z) (z (lambda (x y) x)))(lambda (m) (m 50 60)))
((lambda (m) (m 50 60))(lambda (x y) x))
((lambda (x y) x) 50 60)
50

;3. Dual representation (polar coordinate CS rectilinear coordinate)

;3.1 Rectilinear coordinate
;  constructor: make-complex1
(define (make-complex1 real imag) (cons real imag))
;  destructor: real imag
(define (realr z) (car z))
(define (imagr z) (cdr z))
;  add & multiplicate
(define (addr c1 c2)
  (make-complex1 (+ (realr c1) (realr c2))
             (+ (imagr c1) (imagr c2))))
(define (mulr c1 c2)
  (make-complex1 (- (*(realr c1) (realr c2))
                (*(imagr c1) (imagr c2)))
             (+ (*(realr c1) (imagr c2))
                (*(imagr c1) (realr c2)))))
;  e.g.
(addr (make-complex1 2 3) (make-complex1 4 5))

;3.2 Polar coordinate
;  constructor: make-complex2
(define (make-complex2 norm angle) (cons norm angle))
;  destructor: norm angle
(define (normp z) (car z))
(define (anglep z) (cdr z))
;  add & multiplicate
(define (mulp c1 c2)
  (make-complex2 (*(normp c1)(normp c2))
              (+(anglep c1)(anglep c2))))
;  e.g.
(mulp (make-complex2 2 3) (make-complex2 4 5))

;3.3 Dual Representation

;(define make-rect cons)
;(define real car)
;(define imag cdr)
;(define (make-polar r theta)
;  (make-rect (* r (cos theta))
;             (* r (sin theta))))
;(define (norm c)
;  (sqrt (+ (square (real c))
;           (square (imag c)))))
;(define (angle c)
;  (arctan (/ (imag c) (real c))))

(define make-polar cons)
(define norm car)
(define angle cdr)
(define (make-rect x y)
  (make-polar (sqrt (+ (square x)
                       (square y)))
              (arctan (/ y x))))
(define (real c)
  (* (norm c) (cos (angle c))))
(define (imag c)
  (* (norm c) (sin (angle c))))

(define (square x) (* x x))
(define (arctan x) x)
(define (add c1 c2)
  (make-rect (+ (real c1) (real c2))
            (+ (imag c1) (imag c2))))
(define (mul c1 c2)
  (make-polar (*(norm c1)(norm c2))
              (+(angle c1)(angle c2))))

;e.g.
(real (make-polar 1 3))
(norm (make-rect 2 4))
(add (make-polar 1 3) (make-polar 2 4))
(mul (make-rect 1 3)(make-rect 2 4))