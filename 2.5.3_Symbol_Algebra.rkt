#lang racket
(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get

(define *the-coercion-table* (make-hash));make THE table
(define (put-coercion key1 key2 value) (hash-set! *the-coercion-table* (list key1 key2) value));put
(define (get-coercion key1 key2) (hash-ref *the-coercion-table* (list key1 key2) #f));get

(define (attach-tag type-tag contents)
  (if (number? contents)
       contents
      (cons type-tag contents)
  )
 )
(define (type-tag datum)
  (cond ((pair? datum)(car datum))
       ((number? datum) 'scheme-number) 
      (error "Некорректные помеченные данные -- TYPE-TAG" datum)
  )
)
(define (contents datum)
  (cond ((pair? datum)(cdr datum))
        ((number? datum) datum) 
      (error "Некорректные помеченные данные -- CONTENTS" datum)
   )
 )

(define (apply-op-to-many  op  proc args)
  (if proc
    (let ((res (apply proc (map contents (list (car args) (cadr args))))))
      (if (= (length args) 2)
           res
          (apply-op-to-many  op proc (append (list res) (cddr args))))
     )
    (error "Нет метода для этих типов" (list op args))
  )
)

(define (apply-generic op . args)
  (define (cast-inner list-args list-type-tags new-args cast-type)
    (cond (
      (null? list-args) new-args)
      (else
       (let ((current-type (car list-type-tags)) (current-value (car list-args)) (coercion (get-coercion (car list-type-tags) cast-type)))
        (cond
          ((eq? current-type cast-type) (cast-inner (cdr list-args) (cdr list-type-tags) (append new-args (list current-value)) cast-type))
          (coercion (cast-inner (cdr list-args) (cdr list-type-tags) (append new-args  (list(coercion current-value))) cast-type))
          (else #f)
         )
        )
       )
     )
   )
  (define (find-first-cast type-tags args tail-types)
    (if (null? tail-types)
        ;все перебрали, не нашли рабочее приведение
        #f
        (let ((coerces-args (cast-inner args type-tags '() (car tail-types))))
          (if coerces-args
              ;приведение прошло успешно, возвращаем приведенные аргумены
              coerces-args
              ;не получилось - проверяем следующий тип
              (find-first-cast type-tags args (cdr tail-types))
           )
         )
     )
   )

 (let ((type-tags (map type-tag args)))
  (cond
    ((= (length args) 2)
     (let ((proc (get op type-tags)))
       (if proc (apply proc (map contents args))
           (let ((type1 (car type-tags)) (type2 (cadr type-tags)) (a1 (car args)) (a2 (cadr args)))
             (if (eq? type1 type2) (apply-generic op a1 a2)
                 (let ((t1->t2 (get-coercion type1 type2)) (t2->t1 (get-coercion type2 type1)))
                   (cond 
                     (t1->t2 (apply-generic op (t1->t2 a1) a2))
                     (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                     (else (error "Нет метода для этих типов" (list op type-tags)))
                     )
                   )
                 )
             )        
           )
       )
     )
     ((= (length args) 1)
      (let ((proc (get op type-tags)))
       (if proc (apply proc (map contents args))  (error "Нет метода для этих типов" (list op type-tags)))
       ))
     (else
       ;приводим все к одному типу
        (let ((coerced-values (find-first-cast type-tags args type-tags)))
             (if coerced-values
                 (let ((type-tags (map type-tag coerced-values)))
                  (let ((proc (get op (list (car type-tags) (cadr type-tags)))))
                   ( apply-op-to-many op proc coerced-values)
                  ))
                 (error "Нет метода для этих типов"(list op type-tags))
              )
       )
      )
   )
 )
)


;Действительные числа
(define (install-scheme-number-package)
  (define (tag x)  (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)  (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)  (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)  (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)  (lambda (x y) (tag (/ x y))))
  (put 'equ? '(scheme-number scheme-number)  (lambda (x y) (= x y)))
  (put 'zero? '(scheme-number)  (lambda (x) (= x 0)))
  (put 'make 'scheme-number  (lambda (x) (tag x)))
  (put 'exp '(scheme-number scheme-number) (lambda (x y) (tag (expt x y))))
  (put 'project '(scheme-number) (lambda (x) #f))
  (put 'sqrt '(scheme-number) (lambda (x) (sqrt x)))
  (put 'atan '(scheme-number) (lambda (x) (atan x)))
  (put 'atan '(scheme-number scheme-number) (lambda (x y) (atan x y)))
  (put 'cos '(scheme-number) (lambda (x) (cos x)))
  (put 'sin '(scheme-number) (lambda (x) (sin x)))
  'done
)

(define (install-polynomial-package)
 ;внутренние процедуры
 
 ;Сложение многочленов
 (define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
   (make-poly (variable p1)(add-terms (term-list p1) (term-list p2)))
   (error "Многочлены от разных переменных -- ADD-POLY" (list p1 p2))
  )
 )
 ;Умножение многочленов
 (define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
   (make-poly (variable p1) (mul-terms (term-list p1) (term-list p2)))
   (error "Многочлены от разных переменных -- MUL-POLY" (list p1 p2))
  )
 )
 ; представление poly
 (define (make-poly variable term-list)(cons variable term-list))
 (define (variable p) (car p))
 (define (term-list p) (cdr p))
 (define (variable? x) (symbol? x))
 (define (same-variable? v1 v2)(and (variable? v1) (variable? v2) (eq? v1 v2)))
 
 ; представление термов и списков термов
 (define (adjoin-term term term-list)
  (if (=zero? (coeff term))
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
 (put 'make 'polynomial (lambda (var terms) (tag (make-poly var terms))))
 'done
)

(define (make-polynomial var terms)((get 'make 'polynomial) var terms))
(define (=zero? x) (apply-generic '=zero? x))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (exp x y) (apply-generic 'exp x y))


(install-scheme-number-package)
(install-polynomial-package)