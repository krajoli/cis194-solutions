#lang racket

;; Excercise 1

;; Get the last digit from a number
(define (last-digit n)
  (cond ((= n 0) 0)
        (else (remainder n 10))))

;; Drop the last digit from the number
(define (drop-last-digit n)
  (quotient n 10))


;; Excercise 2

;; Convert an integer into a list of digits in the reverse order
(define (rev-digits n)
  (cond ((<= n 0) '())
        (else (cons (last-digit n) (rev-digits (drop-last-digit n))))))


;; Excercise 3

;; Double every second number in a list starting on the left.
(define (double-every-other l)
  (cond ((empty? l) '())
        ((empty? (cdr l)) l)
        (else (cons (car l) 
                    (cons (* 2 (second l)) 
                          (double-every-other (cdr (cdr l))))))))


;; Excercise 4

;; Calculate the sum of all the digits in every Integer.
(define (sum-digits l)
  (cond ((empty? l) 0)
        (else (+ (apply + (rev-digits (car l))) 
                 (sum-digits (cdr l))))))


;; Excercise 5

;; Validate the credit card number using above functions
(define (luhn n)
  (= (remainder (sum-digits (double-every-other (rev-digits n))) 10) 0))


;; Excercise 6

;; Towers of Hanoi for three pegs
(define (hanoi n p1 p2 p3)
  (cond ((= n 0) '())
        ((= n 1) (list (cons p1 p2)))
        (else (append (hanoi (- n 1) p1 p3 p2)
                    (cons (cons p1 p2) (hanoi (- n 1) p3 p2 p1))))))

  

