#lang racket
; Section 3.1 Assignment and Local state
; Section 3.1.1 Local State Variables

; 1. balance1 is defined in the global environment
(define balance1 100)
(define (withdraw1 amount)
  (if (>= balance1 amount)
      (begin (set! balance1 (- balance1 amount))
             balance1)
      "insufficient funds"))
(withdraw1 20)
(withdraw1 30)
(withdraw1 100)

; 2. balance is defined in a local environment
(define withdraw2
  (let ((balance 200))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "insufficient funds"))))
(withdraw2 20)
(withdraw2 30)
(withdraw2 100)


; 3. make-withdraw creates "withdrawal processors"
(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))
(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))
(W1 20) ;80
(W2 30) ;70
; W1 and W2 are completely independent objects, each with its own local state variable balance

; 4. make-account returns a "bank-account object" with a specified initial balance
; create objects that handle deposits as well as withdrawls, thus represent simple bank account
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  ; dispatch takes a "message" as input and returns one of the two local procedures
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request: MAKE-ACCOUNT"
                       m))))
  ; the dispatch procedure itself is returned as the value that represents the bank-account object
  dispatch)
  ; this is the "message-passing" style of the programming

(define acc (make-account 100))
((acc 'withdraw) 60) ;40
((acc 'deposit) 30) ;70








