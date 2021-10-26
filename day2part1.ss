#! /usr/local/bin/scheme --program

(import (rnrs))
(import (chezscheme))

(define inputstring
  (get-string-all
  (open-input-file "input.txt")))

(define inputlist
  (string->list inputstring))

(define-record-type rightrectangularprism
  (fields
    (immutable l)
    (immutable w)
    (immutable h)))

(define (str-split str ch)
  (let ((len (string-length str)))
    (letrec
      ((split
        (lambda (a b)
          (cond
            ((>= b len) (if (= a b) '() (cons (substring str a b) '())))
              ((char=? ch (string-ref str b)) (if (= a b)
                (split (+ 1 a) (+ 1 b))
                  (cons (substring str a b) (split b b))))
                (else (split a (+ 1 b)))))))
                  (split 0 0))))

(define parsed-rightrectangularprism
  (map
    (lambda (x)
      (apply make-rightrectangularprism x))
  (map 
    (lambda (x) (str-split x #\x))
      (str-split inputstring #\newline))))

(define (wrappingpaper ls) 
  (apply +
    (map 
      (lambda (x) 
	(+ 
	  (+ 
	    (* 2 (string->number (rightrectangularprism-l x)) 
	         (string->number (rightrectangularprism-w x))) 
	    (* 2 (string->number (rightrectangularprism-w x)) 
	         (string->number (rightrectangularprism-h x))) 
	    (* 2 (string->number (rightrectangularprism-h x)) 
	         (string->number (rightrectangularprism-l x)))) 
	  (min 
	    (* (string->number (rightrectangularprism-l x))
	       (string->number (rightrectangularprism-w x)))
	    (* (string->number (rightrectangularprism-w x)) 
	       (string->number (rightrectangularprism-h x))) 
	    (* (string->number (rightrectangularprism-h x)) 
	       (string->number (rightrectangularprism-l x)))))) ls)))
  

(display "The answer to AOC 2015 Day 2 part 1 is: ")
(display (wrappingpaper parsed-rightrectangularprism))
(display "\n")
