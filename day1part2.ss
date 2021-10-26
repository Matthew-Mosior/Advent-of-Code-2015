#! /usr/local/bin/scheme --program

(import (rnrs))
(import (chezscheme))

(define inputstring
  (get-string-all
  (open-input-file "input.txt")))

(define inputlist
  (string->list inputstring))

(define (calcfloor ls)
  (define (iter ls n x)
    (cond
      [(< n 0) x                                            ]
      [(null? ls) x                                         ]
      [(char=? (car ls) #\() (iter (cdr ls) (+ n 1) (+ x 1))] 
      [(char=? (car ls) #\)) (iter (cdr ls) (- n 1) (+ x 1))]
      [else (iter (cdr ls) n (+ x 1))]))
  (iter ls 0 0))

(display "The answer to AOC 2015 Day 1 part 2 is: ")
(display (calcfloor inputlist))
(display "\n")
