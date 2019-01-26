#lang racket
(require racket/include)
(require  "1.3.3_Procs_As_Generics.rkt")
(define (average-damp f)(lambda (x) (average x (f x))))
(define (sqrt x)
(fixed-point (average-damp (lambda (y) (/ x y)))
1.0))