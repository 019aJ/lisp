;2.4.1. Представления комплексных чисел

(define (add-complex z1 z2)
  (make-from-real-imag
    (+ (real-part z1) (real-part z2))
    (+ (imag-part z1) (imag-part z2))
  )
)

(define (sub-complex z1 z2)
  (make-from-real-imag
    (- (real-part z1) (real-part z2))
    (- (imag-part z1) (imag-part z2))
  )
)

(define (mul-complex z1 z2)
  (make-from-mag-ang
    (* (magnitude z1) (magnitude z2))
    (+ (angle z1) (angle z2))
  )
)

(define (div-complex z1 z2)
  (make-from-mag-ang
    (/ (magnitude z1) (magnitude z2))
    (- (angle z1) (angle z2))
  )
)

;Декатрово представление
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z)) (square (imag-part-rectangular z))))
)

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z))
)

(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y))
)

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a))))
)

;Полярное представление
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z)))
)

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z)))
)

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y))) (atan y x)))
)

(define (make-from-mag-ang-polar r a)
  (attach-tag '’'polar (cons r a))
)

;Метки
(define (attach-tag type-tag contents) (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
  (car datum)
  (error "Некорректные помеченные данные -- TYPE-TAG" datum))
)

(define (contents datum)
  (if (pair? datum)
  (cdr datum)
  (error "Некорректные помеченные данные -- CONTENTS" datum))
)

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular)
)

(define (polar? z)
  (eq? (type-tag z) 'polar)
)

;Методы доступа

(define (real-part z)
  (cond
    ((rectangular? z) (real-part-rectangular (contents z)))
    ((polar? z) (real-part-polar (contents z)))
    (else (error "Неизвестный тип -- REAL-PART" z))
  )
)

(define (imag-part z)
  (cond
    ((rectangular? z) (imag-part-rectangular (contents z)))
    ((polar? z) (imag-part-polar (contents z)))
    (else (error "Неизвестный тип -- IMAG-PART" z)))
)

(define (magnitude z)
  (cond
    ((rectangular? z) (magnitude-rectangular (contents z)))
    ((polar? z) (magnitude-polar (contents z)))
    (else (error "Неизвестный тип -- MAGNITUDE" z))
  )
)

(define (angle z)
  (cond
    ((rectangular? z) (angle-rectangular (contents z)))
    ((polar? z) (angle-polar (contents z)))
    (else (error "Неизвестный тип -- ANGLE" z))
  )
)

;Порождающие функции
(define (make-from-real-imag x y)
(make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
(make-from-mag-ang-polar r a))
