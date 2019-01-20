#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")

(require  "2.5.3_Symbol_Algebra_Term_Operations.rkt")
(define (sparse-terms input result)
    (if
     (null? input) result
     (let ((current-term (apply-generic 'first-term input)))
      (if (= (coeff current-term) 0)
       (sparse-terms (cdr input) result)   
       (sparse-terms (cdr input) (append result (list current-term)))
      )
     )  
    )
  )
(define (dense->sparse d-term)
  
  ((get 'make-from-sparse-list 'sparse) (cdr d-term))
)

(put-coercion 'dense 'sparse dense->sparse)

(define (add-terms t1 t2)(apply-generic 'add-terms t1 t2))
(define (sign-term t1)(apply-generic 'sign-term t1))
(define (mul-terms t1 t2)(apply-generic 'mul-terms t1 t2))
(define (empty-termlist? t)(apply-generic 'empty-termlist? t))
(define (first-term t)(apply-generic 'first-term t))
(define (rest-terms t)(apply-generic 'rest-terms t))

(provide (all-defined-out))

