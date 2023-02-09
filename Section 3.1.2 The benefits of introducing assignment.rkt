#lang racket
; Section 3.1 Assignment and Local state
; Section 3.1.2 The benefits of introducing assignment

; rand-update: assume we have rand-update, that if we start with a given number x1 and call rand-update,
;              then the sequence of values x1, x2, x3 will have the desired statistical properties
; x2 = (rand-update x1)
; x3 = (rand-update x2)



; rand: a procedure with a local state variable x
;       each call to "rand" computes rand-update of the current value of x,
;       returns this as the random number, and also stores this as the new value of x
(define rand (let ((x random-init))
               ;local state variable x that is initialized to some fixed value
               (lambda ()
                 (set! x (rand-update x))
                 x)))

; monte-carlo 
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaing 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

; an experiment: cesaro-test to estimate-pi
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))
(define (estimate-pi-1 trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
; assignment encapsulates the state of the random-number generator within the rand procedure


; using rand-update directly rather than rand
;    - could generate the same sequence of random numbers without using assignment by simply calling rand-update directly
;    - but, any part of our program that used random numbers would have to explicitly remember the current value of x to be
;      passed as an argument to rand-update
;    - the random-number generator's insides are leaking out into other parts of the program makes it difficult for us to
;      isolate the Monte Carlo idea so that it can be applied to other tasks
(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))
      
(define (estimate-pi-2 trials)
  (sqrt (/6 (random-gcd-test trials random-init))))

; Conclusion: in a complex process, some parts ahve hidden time-varying local states. If we wish to write computer programs
; whose structure reflects this decomposition, we make computational objects, whose behavior changes with time. We model state
; with local state variables, and we model the changes of state with assignments to those variables. So that we are able to
; structure systems in a more modular fashion than if all states ahd to be manipulated explicitly.


