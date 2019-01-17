#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")
(require  "2.5.3_Symbol_Algebra.rkt")
;Упражнение 2.89.
;Допустим, что мы хотим реализовать систему многочленов, которая эффективна как для плотных,
;так и для разреженных многочленов. Один из способов это сделать заключается в том, чтобы разрешить в системе оба типа представления.
;Перепроектируйте систему с многочленами так, чтобы это обобщение было реализовано.

;Плотные
(define (install-dense-polynomial-package)

  ; внутренние процедуры

  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z) (sqrt-generic (add (square (real-part z)) (square (imag-part z)))))

  (define (angle z) (atan-generic (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a) (cons (mul r (cos-generic a)) (mul r (sin-generic a))))

  ; интерфейс к остальной системе

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(dense) real-part)
  (put 'imag-part '(dense) imag-part)
  (put 'magnitude '(dense) magnitude)
  (put 'angle '(dense) angle)
  (put 'make-from-real-imag dense (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang dense (lambda (r a) (tag (make-from-mag-ang r a))))
)

;Разреженные
(define (install-sparse-polynomial-package)
  ; внутренние процедуры

  (define (magnitude z) (car z))

  (define (angle z) (cdr z))

  (define (make-from-mag-ang r a) (cons r a))

  (define (real-part z)   (mul (magnitude z) (cos-generic (angle z))))

  (define (imag-part z) (mul (magnitude z) (sin-generic (angle z))))

  (define (make-from-real-imag x y) (cons (sqrt-generic (add (square x) (square y))) (atan-generic y x)))

  ; интерфейс к остальной системе

  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar (lambda (r a) (tag (make-from-mag-ang r a))))

  'done
)



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
 (define (terms-zero? terms)
  (if (empty-termlist? terms)
   #t
   (and (zero? (coeff (first-term terms))) (terms-zero? (rest-terms terms)))
  )
 )
(define (poly-zero? p) (terms-zero? (term-list p)))
 ; представление poly
 (define (make-poly variable term-list)(cons variable term-list))
 (define (variable p) (car p))
 (define (term-list p) (cdr p))
 (define (variable? x) (symbol? x))
 (define (same-variable? v1 v2)(and (variable? v1) (variable? v2) (eq? v1 v2)))
 
 ; представление термов и списков термов
 (define (adjoin-term term term-list)
  (if (zero? (coeff term))
   term-list
   (cons term term-list)
  )
 )
 (define (the-empty-termlist) '())
 (define (first-term term-list) (car term-list))
 (define (rest-terms term-list) (cdr term-list))
 (define (empty-termlist? term-list) (null? term-list))
 (define (make-term order coeff) (list order coeff))
 (define (order term) (car term))
 (define (coeff term) (cadr term))
 (define (sign-term L)
  (cond 
   ((empty-termlist? L) L)
   (else
    (let ((t1 (first-term L)) )
     (adjoin-term (make-term (order t1)(- 0 (coeff t1) )) (sign-term (rest-terms L)))
     )
    )
   )
  )
 
 (define (add-terms L1 L2)
  (cond 
   ((empty-termlist? L1) L2)
   ((empty-termlist? L2) L1)
   (else
    (let ((t1 (first-term L1)) (t2 (first-term L2)))
     (cond 
      ((> (order t1) (order t2)) (adjoin-term t1 (add-terms (rest-terms L1) L2)))
      ((< (order t1) (order t2)) (adjoin-term t2 (add-terms L1 (rest-terms L2))))
      (else (adjoin-term (make-term (order t1)(add (coeff t1) (coeff t2))) (add-terms (rest-terms L1) (rest-terms L2))))
     )
    )
   )
  )
 )
 (define (mul-terms L1 L2)
  (if (empty-termlist? L1)
   (the-empty-termlist)
   (add-terms (mul-term-by-all-terms (first-term L1) L2)(mul-terms (rest-terms L1) L2))
  )
 )
 (define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
   (the-empty-termlist)
   (let ((t2 (first-term L)))
    (adjoin-term (make-term (+ (order t1) (order t2))(mul (coeff t1) (coeff t2))) (mul-term-by-all-terms t1 (rest-terms L)))
   )
  )
 )

 ;интерфейс к остальной системе
 (define (tag p) (attach-tag 'polynomial p))
 (put 'add '(polynomial polynomial)(lambda (p1 p2) (tag (add-poly p1 p2))))
 (put 'mul '(polynomial polynomial)(lambda (p1 p2) (tag (mul-poly p1 p2))))
 (put 'sub '(polynomial polynomial)(lambda (p1 p2) (tag (sub-poly p1 p2))))
 (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
 (put 'make-term 'polynomial (lambda (order coef) (make-term order coef)))
 (put 'zero? '(polynomial) (lambda (p) (poly-zero? p)))
 (put 'sign '(polynomial) (lambda (p) (make-polynomial (variable p)(sign-term (term-list p)))))
 'done
)

(define (make-polynomial var terms)((get 'make 'polynomial) var terms))
(define (make-term order coef)((get 'make-term 'polynomial) order coef))
(define (sign p)(apply-generic 'sign p))
(install-polynomial-package)

(provide (all-defined-out))

