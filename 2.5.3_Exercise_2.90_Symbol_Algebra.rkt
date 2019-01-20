#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")
(require  "2.5.3_Symbol_Algebra_Term_Operations.rkt")
(require  "2.5.3_Symbol_Algebra_Dense_Term.rkt")
(require  "2.5.3_Symbol_Algebra_Sparse_Term.rkt")
(require  "2.5.3_Symbol_Algebra_Term_Coercion.rkt")
;Упражнение 2.89.
;Допустим, что мы хотим реализовать систему многочленов, которая эффективна как для плотных,
;так и для разреженных многочленов. Один из способов это сделать заключается в том, чтобы разрешить в системе оба типа представления.
;Перепроектируйте систему с многочленами так, чтобы это обобщение было реализовано.

(define (install-polynomial-package)
 ;внутренние процедуры
 (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
   (make-poly (variable p1)(add-terms (term-list p1) (term-list p2)))
   (error "Многочлены от разных переменных -- ADD-POLY" (list p1 p2))
  )
 )
 (define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
   (make-poly (variable p1)(add-terms (term-list p1) (sign-term (term-list p2))))
   (error "Многочлены от разных переменных -- SUB-POLY" (list p1 p2))
  )
 )
 (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
   (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
   (error "Многочлены от разных переменных -- MUL-POLY" (list p1 p2))
  )
 )
 (define (sign-poly p1)
   (make-poly (variable p1) (sign-term (term-list p1)))
 )
 (define (terms-zero? terms)
  (if (empty-termlist? terms)
   #t
   (and (zero? (coeff (first-term terms))) (terms-zero? (rest-terms terms)))
  )
 )
 (define (make-form-dense var dense-list)
  (make-poly var ((get 'make-from-dense-list 'dense) dense-list))
 )
 (define (make-form-sparse var sparse-list)
  (make-poly var ((get 'make-from-sparse-list 'sparse) sparse-list))
 ) 
  
(define (poly-zero? p) (terms-zero? (term-list p)))
 ; представление poly
 (define (make-poly variable term-list)(cons variable term-list))
 (define (variable p) (car p))
 (define (term-list p) (cdr p))
 (define (variable? x) (symbol? x))
 (define (same-variable? v1 v2)(and (variable? v1) (variable? v2) (eq? v1 v2)))
 
 ;интерфейс к остальной системе
 (define (tag p) (attach-tag 'polynomial p))
 (put 'add '(polynomial polynomial)(lambda (p1 p2) (tag (add-poly p1 p2))))
 (put 'mul '(polynomial polynomial)(lambda (p1 p2) (tag (mul-poly p1 p2))))
 (put 'sub '(polynomial polynomial)(lambda (p1 p2) (tag (sub-poly p1 p2))))
 (put 'make-form-dense 'polynomial (lambda (var dense-list) (tag (make-form-dense var dense-list))))
 (put 'make-form-sparse 'polynomial (lambda (var sparse-list) (tag (make-form-sparse var sparse-list))))
 (put 'zero? '(polynomial) (lambda (p) (poly-zero? p)))
 (put 'sign '(polynomial) (lambda (p) (tag (sign-poly p))))
 'done
)

(define (make-polynomial-form-dense var terms)((get 'make-form-dense 'polynomial) var terms))
(define (make-polynomial-form-sparse var terms)((get 'make-form-sparse 'polynomial) var terms))

(install-polynomial-package)

(provide (all-defined-out))


(define sp-term-source
  (list (make-term 100 1)
        (make-term   2 2)
        (make-term   0 1)))
(define d-term-source
  (list 1 2 1))

(define d-list
  ((get 'make-from-dense-list 'dense) d-term-source))
(define sp-list
  ((get 'make-from-sparse-list 'sparse) sp-term-source))

(define pt1 (make-polynomial-form-sparse 'x sp-term-source))
(define pt2 (make-polynomial-form-dense 'x d-term-source))
(display "pt1 = ")(display pt1)(newline)
(display "pt2 = ")(display pt2)(newline)
(display "sum of ")(display pt1)(display " and ")(display pt2)(display " is ")(display (add pt1 pt2))(newline)
(display "mult of ")(display pt1)(display " and ")(display  pt2)(display " is ")(display (mul pt1  pt2))(newline)

(newline)