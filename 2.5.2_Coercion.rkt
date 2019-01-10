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
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 (car type-tags)) (type2 (cadr type-tags)) (a1 (car args)) (a2 (cadr args)))
                                          (if (eq? type1 type2)
                                              (apply-generic op a1 a2)
            (let ((t1->t2 (get-coercion type1 type2)) (t2->t1 (get-coercion type2 type1)))
              (cond 
                (t1->t2 (apply-generic op (t1->t2 a1) a2))
                (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                (else (error "Нет метода для этих типов" (list op type-tags)))
              )
            )
                                           )
          )
          (error "Нет метода для этих типов"(list op type-tags))
        )
      )
    )
  )
)



;декартовы координаты
(define (install-rectangular-package)

  ; внутренние процедуры

  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z) (sqrt-generic (add (square (real-part z)) (square (imag-part z)))))

  (define (angle z) (atan-generic (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a) (cons (mul r (cos-generic a)) (mul r (sin-generic a))))

  ; интерфейс к остальной системе

  (define (tag x) (attach-tag 'rectangular x))

  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular (lambda (r a) (tag (make-from-mag-ang r a))))

  'done
)

;Полярные координаты
(define (install-polar-package)
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
  (put 'raise '(scheme-number) (lambda (x) (make-rational x 1)))
  (put 'project '(scheme-number) (lambda (x) #f))
  (put 'sqrt '(scheme-number) (lambda (x) (sqrt x)))
  (put 'atan '(scheme-number) (lambda (x) (atan x)))
  (put 'atan '(scheme-number scheme-number) (lambda (x y) (atan x y)))
  (put 'cos '(scheme-number) (lambda (x) (cos x)))
  (put 'sin '(scheme-number) (lambda (x) (sin x)))
  'done
)


;Рациональные числа
(define (install-rational-package)
  ; внутренние процедуры
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
    (cons (/ n g) (/ d g)))
  )
  (define (add-rat x y)(make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (sub-rat x y)(make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x))) (* (denom x) (denom y))))
  (define (mul-rat x y)(make-rat (* (numer x) (numer y))  (* (denom x) (denom y))))
  (define (div-rat x y)(make-rat (* (numer x) (denom y))  (* (denom x) (numer y))))
  ; интерфейс к остальной системе
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)  (lambda (x y) (and (= (numer x) (numer y )) (= (denom x) (denom y )))))
  (put 'zero? '(rational) (lambda (x)  (and (= (numer x) 0) (= (denom x) 1))))
  (put 'raise '(rational) (lambda (x) (make-complex-from-real-imag (tag x) 0)))
  (put 'project '(rational) (lambda (x) (if (= (denom x) 1) (numer x) #f)))
  ;в результате этих операций число становится действительным
  (put 'sqrt '(rational) (lambda (x) (/ (sqrt (numer x)) (sqrt (denom x)))))
  (put 'atan '(rational) (lambda (x) (atan (denom x) (numer x))))
  (put 'atan '(rational rational) (lambda (x y) (atan-one-arg-generic (tag (div-rat y x)))))
  (put 'cos '(rational) (lambda (x) (cos (/ (numer x)(denom x) ))))
  (put 'sin '(rational) (lambda (x) (sin (/ (numer x)(denom x) ))))
  'done
)


;Комплексные числа
(define (install-complex-package)
  ; процедуры, импортируемые из декартова и полярного пакетов
  (define (make-from-real-imag x y) ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a) ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (angle z) (apply-generic 'angle z))
  (define (magnitude z) (apply-generic 'magnitude z))
  ; внутренние процедуры
  (define (add-complex z1 z2) (make-from-real-imag (add (real-part z1) (real-part z2)) (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (sub (real-part z1) (real-part z2)) (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (mul (magnitude z1) (magnitude z2)) (add (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (div (magnitude z1) (magnitude z2)) (sub (angle z1) (angle z2))))
  ; интерфейс к остальной системе
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'real-part '(complex) (lambda (z)(real-part z)))
  (put 'imag-part '(complex) (lambda (z)(imag-part z)) )
  (put 'magnitude '(complex) (lambda (z)(magnitude z)))
  (put 'angle '(complex) (lambda (z)(angle z)))
  (put 'equ? '(complex complex)  (lambda (x y) (and (= (real-part x) (real-part y )) (= (imag-part x) (imag-part y )))))
  (put 'zero? '(complex)  (lambda (x)  (and (= (real-part x) 0) (= (imag-part x) 0))))
  (put 'project '(complex)  (lambda (x)  (if (= (imag-part x) 0) (real-part x) #f)))
  'done
)
;Обобщенные арифметические процедуры определяются следующим образом:
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
(define (exp x y) (apply-generic 'exp x y))
(define (make-rational n d) ((get 'make 'rational) n d))
(define (make-scheme-number n)((get 'make 'scheme-number) n))
(define (make-complex-from-real-imag x y)((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)((get 'make-from-mag-ang 'complex) r a))
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))
(define (square a)(mul a a))
(define (sqrt-generic a)(apply-generic 'sqrt a))
(define (atan-one-arg-generic a)(apply-generic 'atan a))
(define (atan-generic x y)(apply-generic 'atan x y))
(define (cos-generic a)(apply-generic 'cos a))
(define (sin-generic a)(apply-generic 'sin a))

;Приведение типов
(define (scheme-number->complex n)(make-complex-from-real-imag (contents n) 0))
(put-coercion 'scheme-number 'complex scheme-number->complex)
(define (scheme-number->rational n)(make-rational (contents n) 1))
(put-coercion 'scheme-number 'rational scheme-number->rational)
(define (rational->complex n)(make-complex-from-real-imag n 0))
(put-coercion 'rational 'complex rational->complex)
(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

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

(provide (all-defined-out))