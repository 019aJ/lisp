#lang racket
(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get
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
        (error
        "Нет метода для этих типов -- APPLY-GENERIC"
        (list op type-tags))
      )
    )
  )
)

(define (square a)(* a a))

;декартовы координаты
(define (install-rectangular-package)

  ; внутренние процедуры

  (define (real-part z) (car z))

  (define (imag-part z) (cdr z))

  (define (make-from-real-imag x y) (cons x y))

  (define (magnitude z) (sqrt (+ (square (real-part z))(square (imag-part z)))))

  (define (angle z) (atan (imag-part z) (real-part z)))

  (define (make-from-mag-ang r a) (cons (* r (cos a)) (* r (sin a))))

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

  (define (real-part z)   (* (magnitude z) (cos (angle z))))

  (define (imag-part z) (* (magnitude z) (sin (angle z))))

  (define (make-from-real-imag x y) (cons (sqrt (+ (square x) (square y))) (atan y x)))

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

;Обобщенные арифметические процедуры определяются следующим образом:
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (zero? x) (apply-generic 'zero? x))
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
  'done
)
(define (make-scheme-number n)((get 'make 'scheme-number) n))

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
  'done
)
(define (make-rational n d) ((get 'make 'rational) n d))

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
  (define (add-complex z1 z2) (make-from-real-imag (+ (real-part z1) (real-part z2)) (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2) (make-from-real-imag (- (real-part z1) (real-part z2)) (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2) (make-from-mag-ang (* (magnitude z1) (magnitude z2)) (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2) (make-from-mag-ang (/ (magnitude z1) (magnitude z2)) (- (angle z1) (angle z2))))
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
  'done
)
(install-polar-package)
(install-rectangular-package)
(install-scheme-number-package)
(install-rational-package)
(install-complex-package)

;Упражнение 2.77.
;Хьюго Дум пытается вычислить выражение (magnitude z), где z — комплексный объект.
;К своему удивлению, вместо ответа 5 он получает сообщение об ошибке от applygeneric, гласящее, что у операции magnitude нет методов для типа (complex)
;исправьте пакет
(define (make-complex-from-real-imag x y)((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)((get 'make-from-mag-ang 'complex) r a))
(define a (make-complex-from-real-imag 4 3))
(apply-generic 'magnitude a)
(apply-generic 'angle a)
(apply-generic 'real-part a)
(apply-generic 'imag-part a)
(add a a)

;Упражнение 2.78.
;В пакете scheme-number внутренние процедуры, в сущности, ничего не делают, только вызывают
;элементарные процедуры +, -, и т.д. Прямо использовать примитивы языка не было возможности, поскольку наша система меток типов требует, чтобы каждый объект данных был снабжен меткой.
;Однако на самом деле все реализации Лиспа имеют систему типов, которую они используют внутри себя. Элементарные процедуры вроде symbol? или number? определяют, относится ли объект
;к определенному типу. Измените определения type-tag, contents и attach-tag так, чтобы наша обобщенная система использовала внутреннюю систему типов Scheme.
;То есть, система должна работать так же, как раньше, но только обычные числа должны быть представлены просто в виде чисел языка Scheme, а не в виде пары, у которой первый элемент
;символ scheme-number.
(add 4 5)

;Упражнение 2.79.
;Определите обобщенный предикат равенства equ?, который проверяет два числа на равенство,
;и вставьте его в пакет обобщенной арифметики. Операция должна работать для обычных чисел, рациональных и комплексных.
(display "Процедура проверки на равенство")
(newline)
(display "Рациональные:")
(newline)
(display "(equ? 4 5)")
(newline)
(equ? 4 5)

(display "Действительные:")
(newline)
(define r_a (make-rational 1 3))
(define r_b (make-rational 1 5))
(define r_c (make-rational 1 3))
(define r_d (make-rational 2 3))
(display "1/3 1/5")
(newline)
(equ? r_a r_b)
(display "1/3 1/3")
(newline)
(equ? r_a r_c)
(display "1/3 2/3")
(newline)
(equ? r_a r_d)

(display "Комплексные:")
(newline)
(define c_a (make-complex-from-real-imag 1 3))
(define c_b (make-complex-from-real-imag 1 5))
(define c_c (make-complex-from-real-imag 1 3))
(define c_d (make-complex-from-real-imag 2 3))
(display "1+3i 1+5i")
(newline)
(equ? c_a c_b)
(display "1+3i 1+3i")
(newline)
(equ? c_a c_c)
(display "1+3i 2+3i")
(newline)
(equ? c_a c_d)

;Упражнение 2.80.
;Определите обобщенный предикат =zero?, который проверяет, равен ли его аргумент нулю, и
;вставьте его в пакет обобщенной арифметики. Предикат должен работать для обычных, рациональных и комплексных чисел.
(display "Проверка на ноль")
(newline)
(display "Рациональные:")
(newline)
(display "0: ")
(zero? 0)
(display "5: ")
(zero? 5)
(display "Действительные:")
(newline)
(define r_zero (make-rational 0 1))
(display "1/3: ")
(zero? r_a)
(display "0: ")
(zero? r_zero)
(display "Комплексные:")
(newline)
(define c_zero (make-complex-from-real-imag 0 0))
(display "1+3i: ")
(zero? c_a )
(display "0: ")
(zero? c_zero)