#lang racket
;; this is the code for problem set -- Lunar Lander

; Updating the ship state through one time unit when given a (constant) burn rate
;  You'll need to modify this procedure
(define (update ship-state fuel-burn-rate)
  {let ((real-burn-rate
         (if (< (/ (fuel ship-state) dt) fuel-burn-rate)
             (min (/ (fuel ship-state) dt) 1)
             (if (> fuel-burn-rate 1)
                 1
                 fuel-burn-rate))))  
  (make-ship-state
   (+ (height ship-state) (* (velocity ship-state) dt)) ; height
   (+ (velocity ship-state)  
      (* (- (* engine-strength real-burn-rate) gravity)
         dt))  ; velocity
   (- (fuel ship-state) (* real-burn-rate dt)))})  ; fuel

; How to begin the "game"
;  You'll need to modify this procedure
(define (play strategy) (lander-loop (initial-ship-state) strategy))

; Basic loop for the "game"
;  You'll need to modify this procedure
(define (lander-loop ship-state strategy)
  (show-ship-state ship-state) ; Display the current state
  (if (landed? ship-state) ; Run the next step 
      (end-game ship-state)
      (lander-loop (update ship-state (strategy ship-state)) strategy)))

; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!! WRITE YOUR NEW PROCEDURES HERE !!!!!!!!!!!!!!
; !!!!!! (this includes code-based exercise solutions!) !!!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

; Homework Exercises 1

; Exercise 1.17 Fast product based on fast exponentiation
(define(fast-mult a b)
  (define (double x)
    (* x 2))
  (define (halve x)
    (/ x 2))
  (define (even? n)
    (= (remainder n 2) 0))
  (cond ((= b 1) a)
     ((even? b)(double (fast-mult a (halve b))))
     (else (+ a (fast-mult a (- b 1))))))

;Exercise 1.43: Repeated composition of a function
(define (square x) (* x x))
(define (compose f g)
  (lambda (x) (f(g x))))
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

;Exercise 1.44: Smoothing a function
(define (smooth f)
  (define dx 0.001)
  (define (average x1 x2 x3)
    (/ (+ x1 x2 x3) 3))
  (lambda(x) (average (f (- x dx)) (f x) (f (+ x dx)))))

(define (n_fold_smooth n)
  (repeated smooth n))

; 2 Laboratory Assignment: Lunar Lander

;strategy 0 (original): ask-user
(define (ask-user ship-state) (get-burn-rate))

;strategy 1: full-burn (Problem 2)
(define (full-burn ship-state) 1)

;strategy 2: no-burn (Problem 2)
(define (no-burn ship-state) 0)

;strategy 3: random-choice (Problem 3)
(define (random-choice strategy-1 strategy-2)
  (lambda (ship-state)
    (if (= (random 2) 0)
        (strategy-1 ship-state)
        (strategy-2 ship-state))))

;generalized choice (Problem 5)
(define (choice strategy-1 strategy-2 predicate)
  (lambda (ship-state)
    (cond ((predicate ship-state) (strategy-1 ship-state))
          (else (strategy-2 ship-state)))))

;strategy 4: height-choice (generalized choice) (Problem 4&5)
(define (height-choice strategy-1 strategy-2 breakline)
  (choice strategy-1
          strategy-2
          (lambda (ship-state) (> (height ship-state) breakline))))

;strategy 5: height-random-choice (Problem 6)
(define (height-random-choice)
  (choice no-burn
          (random-choice full-burn ask-user)
          (lambda (ship-state) (> (height ship-state) 40))))

;strategy 6: constant-acc (Problem 8)
(define (constant-acc ship-state)
  (/ (+ gravity (/ (* (velocity ship-state) (velocity ship-state))
                   (* 2 (height ship-state)))) engine-strength))

;strategy 7: optimal-constant-acc (Problem 11)
(define (optimal-constant-acc)
  (choice no-burn
          constant-acc
          (lambda (ship-state) (< (/ (+ gravity (/ (* (velocity ship-state) (velocity ship-state))
                   (* 2 (height ship-state)))) engine-strength) 0.95))))

;strategy 8: fuel-optimal-constant-acc (Problem 12)
(define (fuel-optimal-constant-acceleration)
  (let ((deadheight (*(/ (* gravity
                          (height (initial-ship-state)))
                       engine-strength) 1.05)))
  (height-choice no-burn constant-acc deadheight)))
    
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; !!!!!!!!!!!!!!!!!!!!! WRITE NEW CODE ABOVE HERE !!!!!!!!!!!!!!!!!!!!!
; !!!!! (code below here is still useful to read and understand!) !!!!!
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

; Writing the ship's state to console
(define (show-ship-state ship-state)
  (write-line
   (list 'height (height ship-state)
         'velocity (velocity ship-state)
         'fuel (fuel ship-state))))

; Determining if the ship has hit the ground
(define (landed? ship-state)
  (<= (height ship-state) 0))

; Ending the game
(define (end-game ship-state)
  (let ((final-velocity (velocity ship-state)))
    (write-line final-velocity)
    (cond ((>= final-velocity safe-velocity)
           (write-line "good landing")
           'game-over)
          (else
           (write-line "you crashed!")
           'game-over))))

; Used in player-controlled burn strategy
(define (get-burn-rate)
  (if (= (player-input) burn-key)
      1
      0))

; Starting state of the ship
(define (initial-ship-state)
  (make-ship-state
   50  ; 50 km high
   0   ; not moving (0 km/sec)
   20)); 20 kg of fuel left

; Global constants for the "game"
(define engine-strength 1) ; 1 kilonewton-second
(define safe-velocity -0.5) ; 0.5 km/sec or faster is a crash
(define burn-key 32) ;space key
(define gravity 0.5) ; 0.5 km/sec/sec
(define dt 0.3) ; 1 second interval of simulation

; Getting the player's input from the console
(define (player-input)
  (char->integer (prompt-for-command-char " action: ")))


; You’ll learn about the stuff below here in Chapter 2. For now, think of make-ship-state,
; height, velocity, and fuel as primitive procedures built in to Scheme.
(define (make-ship-state height velocity fuel)
  (list 'HEIGHT height 'VELOCITY velocity 'FUEL fuel))
(define (height state) (second state))
(define (velocity state) (fourth state))
(define (fuel state) (sixth state))
(define (second l) (cadr l)) 
(define (fourth l) (cadr (cddr l)))
(define (sixth l) (cadr (cddr (cddr l))))
; Users of DrScheme or DrRacket: add these for compatibility with MIT Scheme...
; for input and output
(define (write-line x)
  (display x)
  (newline))
; Fixes the buffer-reading issue- you can ignore this method
(define (input-parse line)
  (if (or (equal? "" line) (not (equal? " " (substring line 0 1))))
      #\newline
      #\space))
(define (prompt-for-command-char prompt)
  (display prompt)
  (input-parse (read-line)))
; for random number generation
(#%require (only racket/base random))
; a ridiculous addendum
; (you’ll need this for the exercises)
(define (1+ x) (+ 1 x))