#lang racket
(require racket/include)

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(provide (all-defined-out))

