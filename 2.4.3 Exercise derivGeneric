#lang racket
(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get
(define (attach-tag type-tag contents)(cons type-tag contents))
;Упражнение 2.73.
;В разделе 2.3.2 описывается программа, которая осуществляет символьное дифференцирование:
(define (power x n)
 (define (power-inner x n rem)
   (cond ((= n 0) rem)
         (else (power-inner x (- n 1) (* rem x)))
   )
 )
 (power-inner x n 1)
)

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)(and (number? exp) (= exp num)))
(define (sum? x)(and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (if (null? (cdddr s))(caddr s)(cons '+ (cddr s))))
(define (product? x)(and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (if (null? (cdddr p))(caddr p)(cons '* (cddr p))))
(define (exponentiation? x)(and (pair? x) (eq? (car x) '**)))
(define (base p) (cadr p))
(define (exponent p) (caddr p))
(define (make-product m1 m2)
 (cond ((or (=number? m1 0) (=number? m2 0)) 0)
 ((=number? m1 1) m2)
 ((=number? m2 1) m1)
 ((and (number? m1) (number? m2)) (* m1 m2))
 ((and (number? m1) (product? m2) (number? (make-product (multiplier m2) (multiplicand m2)))) (+ m1 (make-product (multiplier m2) (multiplicand m2))))
 (else (list '* m1 m2)))
)
(define (make-exponentiation b e)
 (cond ((=number? e 0) 1)
 ((=number? b 0) 0)
 ((=number? b 1) b)
 ((=number? e 1) b)
 ((and (number? b) (number? e)) (power b e))
 (else (list '** b e)))
)
(define (deriv exp var)
 (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   ((sum? exp) (make-sum (deriv (addend exp) var) (deriv (augend exp) var)))
   ((product? exp)
     (make-sum
     (make-product (multiplier exp)
     (deriv (multiplicand exp) var))
     (make-product (deriv (multiplier exp) var)
     (multiplicand exp))))
   ((exponentiation? exp)
     (make-product
       (exponent exp)
       (make-product
         (make-exponentiation (base exp) (- (exponent exp) 1))
         (deriv (base exp) var)) )
   )
   (else (error "неизвестный тип выражения -- DERIV" exp))
 )
)
;Можно считать, что эта программа осуществляет диспетчеризацию по типу выражения, которое
;требуется продифференцировать. В этом случае «меткой типа» элемента данных является символ
;алгебраической операции (например, +), а операция, которую нужно применить – deriv. Эту программу можно преобразовать в управляемый данными стиль, если переписать основную процедуру
;взятия производной в виде
(define (deriv-generic exp var)
 (cond
   ((number? exp) 0)
   ((variable? exp) (if (same-variable? exp var) 1 0))
   (else ((get 'deriv (operator exp)) (operands exp) var))
 )
)
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
;а. Объясните, что происходит в приведенном фрагменте кода. Почему нельзя включить в операцию выбора, управляемого данными, предикаты number? и variable?
;б. Напишите процедуры для вычисления производных от суммы и произведения, а также дополнительный код, чтобы добавить их к таблице, которой пользуется приведенный фрагмент.

(define (install-sum-package)
 (define (make-sum a1 a2) (cons a1 a2))
 (define (addend s) (cadr s))
 (define (augend s) (caddr s))
 (define (deriv-sum s) (make-sum (deriv (addend s)) (deriv (augend s))))
 (define (tag x) (attach-tag '+ x))
 (put 'deriv '(+) deriv-sum)
 (put 'make-sum '+ (lambda (x y) (tag (make-sum x y))))
 'done
)
(define (make-sum x y) ((get 'make-sum '+) x y))

(define (install-multiply-package)
 (define (make-multiply a1 a2) (cons a1 a2))
 (define (multiplier p) (cadr p))
 (define (multiplicand p) (if (null? (cdddr p))(caddr p)(cons '* (cddr p))))
 (define (deriv-multiply var)
   (make-sum
     (make-multiply (multiplier exp)
     (deriv (multiplicand exp) var))
     (make-multiply (deriv (multiplier exp) var)
     (multiplicand exp))
   )
 )
 (define (tag x) (attach-tag '+ x))
 (put 'deriv '(*) deriv-multiply)
 (put 'make-multiply '* (lambda (x y) (tag (make-multiply x y))))
 'done
)
(define (make-multiply x y) ((get 'make-multiply '*) x y))

;в. Выберите еще какое-нибудь правило дифференцирования, например для возведения в степень
;(упражнение 2.56), и установите его в систему.
(define (install-power-package)
 (define (make-power a1 a2) (cons a1 a2))
 (define (base p) (cadr p))
 (define (exponent p) (caddr p))
 (define (deriv-power var)
     (make-multiply
       (exponent exp)
       (make-multiply
         (make-exponentiation (base exp) (- (exponent exp) 1))
         (deriv (base exp) var)) )
 )
 (define (tag x) (attach-tag '+ x))
 (put 'deriv '(**) deriv-power)
 (put 'make-power '** (lambda (x y) (tag (make-power x y))))
 'done
)
(define (make-power x y) ((get 'make-power '**) x y))

(install-sum-package)
(install-multiply-package)
(install-power-package)



(deriv '(+ x x x) 'x)
(deriv '(* x x x) 'x)
(deriv '(+ x (* x  (+ x (+ y 2)))) 'x)
(deriv '(** x 3) 'x)
(deriv '(+ (** x 3) x) 'x) 
