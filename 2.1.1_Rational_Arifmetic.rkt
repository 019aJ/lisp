#lang racket
(define (add-rat x y)
(make-rat (+ (* (numer x) (denom y))
(* (numer y) (denom x)))
(* (denom x) (denom y))))

(define (sub-rat x y)
(make-rat (- (* (numer x) (denom y))
(* (numer y) (denom x)))
(* (denom x) (denom y))))

(define (mul-rat x y)
(make-rat (* (numer x) (numer y))
(* (denom x) (denom y))))

(define (div-rat x y)
(make-rat (* (numer x) (denom y))
(* (denom x) (numer y))))

(define (equal-rat? x y)
(= (* (numer x) (denom y))
(* (numer y) (denom x))))

(define (make-rat n d)
  (if ( <= (* n d) 0)
   (cons (- (abs n)) (abs d))
   (cons (abs n) (abs d))
  )
)
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)
;Упражнение 2.1.
;Make-rat должна нормализовывать знак так, чтобы в случае, если
;рациональное число положительно, то и его числитель, и знаменатель были бы положительны, а
;если оно отрицательно, то чтобы только его числитель был отрицателен.

(make-rat 2 -1)