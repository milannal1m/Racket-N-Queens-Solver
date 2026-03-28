#lang racket
;; n-queens
;; place n queens on a n*n board, without either attacking the other
;; n: number of queens
;; k: current column queen try 

(define (print-board lst n)
  (cond ((null? lst)(newline))
      (else
       (print-row (car lst) n)
       (print-board (cdr lst) n))))

(define (print-row pos n)
  (cond((equal? pos 1) (display "Q"))
       (else (display "#")))
  (cond((equal? n 1) (newline))
       (else (print-row (- pos 1) (- n 1)))))

(define (column-check lst k n)
  (if (equal? 0 k) (column-check (cdr lst) (-(car lst)1) n)
          (if (member k lst) (column-check lst (- k 1) n)
              (if(diagonal-check? lst k n 1)(cons k lst)
                 (column-check lst (- k 1) n )))))

(define (diagonal-check? lst k n i)
  (if (null? lst) #t
      (if (equal? (abs(- k (car lst)))i) #f
          (diagonal-check? (cdr lst) k n (+ i 1)))))


(define (solve-n-queens2 lst n)
  (if (equal? (length lst) n)(begin (display lst) (newline) (print-board lst n))
      (solve-n-queens2 (column-check lst n n)n)))

(define (solve-n-queens n)
  (if (< n 4) (display "ERROR: Unsolvable Problem :P")
      (solve-n-queens2 '() n)))

(solve-n-queens 27)