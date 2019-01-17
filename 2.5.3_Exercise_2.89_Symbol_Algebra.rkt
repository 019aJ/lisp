#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")
(require  "2.5.3_Symbol_Algebra.rkt")
;Упражнение 2.89.
;Определите процедуры, которые реализуют представление в виде списка термов, описанное выше
;как подходящее для плотных многочленов.

; представление термов и списков термов
 (define (adjoin-term term term-list)
  (define (find-place upward-list below-list)
   (let ((input-order (order term)) (input-coef (coeff term)) (current-order (max-order below-list)))
    (if (= input-order current-order)
     (append upward-list (cons (+ input-coef (car below-list)) (cdr below-list)))
     (find-place (append upward-list (list (car below-list))) (cdr  below-list))
    )
   )
  )
  (let ((input-order (order term)) (input-coef (coeff term)) (term-list-length (length term-list))) 
   (cond
    ((zero? (coeff term)) term-list)
    ((= input-order term-list-length) (cons (coeff term) term-list))
    ;Добить нулями
    ((> input-order term-list-length) (adjoin-term term (cons 0 term-list)))
    ;Найти место для коэфицента и добавить
    (else (find-place '() term-list))
   )
  )
 )


 (define (the-empty-termlist) '())
 (define (max-order term-list) (- (length term-list) 1))
 (define (first-term term-list) (make-term (max-order term-list)(car term-list)))
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

(define t1
 (adjoin-term (make-term 4 1)
  (adjoin-term  (make-term 3 2) 
   (adjoin-term  (make-term 2 -5)
    (adjoin-term  (make-term 1 3)
     (adjoin-term (make-term 0 7) (the-empty-termlist)))
    )
   )
 )
)

(define t2 (adjoin-term (make-term 2 2) (the-empty-termlist)))
(define t3 (adjoin-term (make-term 3 3) (the-empty-termlist)))

(display "2x^2 + 3x^3 = ")(display (add-terms t2 t3)) (newline)
(display "2x^2 * 3x^3 = ")(display (mul-terms t2 t3)) (newline)
(display "2 * (x^4 + + 2x^3 - 5x^2 +3x + 7) = ")(display (add-terms t1 t1)) (newline)