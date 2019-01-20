#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")
(require  "2.5.3_Symbol_Algebra_Dense_Term.rkt")
(require  "2.5.3_Symbol_Algebra_Sparse_Term.rkt")

(define (dense->sparse term)(make-complex-from-real-imag (contents n) 0))
(put-coercion 'dense 'sparse dense->sparse)

(provide (all-defined-out))

