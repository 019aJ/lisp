#lang racket
;Алгоритм евклида нахождения наибольшего общего делителя
(define (gcd a b)
 (if (= b 0)
  a
  (gcd b (remainder a b))
 )
)