#! /usr/local/bin/scheme --program

(import (rnrs))
(import (chezscheme))

(define inputstring
  (get-string-all
  (open-input-file "input.txt")))

(define inputlist
  (string->list inputstring))

(define (calcfloor ls)
  (define (iter ls n)
    (cond
      [(null? ls) n]
      [(char=? (car ls) #\() (iter (cdr ls) (+ n 1))]
      [(char=? (car ls) #\)) (iter (cdr ls) (- n 1))]
      [else (iter (cdr ls) n)]))
  (iter ls 0))

(display "The answer to AOC 2015 Day 1 is: ")
(display (calcfloor inputlist))
(display "\n")
