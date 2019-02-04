#lang racket
(define (add-rat x y)
(make-rat (+ (* (numer x) (denom y))
(* (numer y) (denom x)))
(* (denom x) (denom y))))

(define (sub-rat x y)
(make-rat (- (* (numer x) (denom y))
(* (numer y) (denom x)))
(* (denom x) (denom y))))

(define (mul-rat x y)
(make-rat (* (numer x) (numer y))
(* (denom x) (denom y))))

(define (div-rat x y)
(make-rat (* (numer x) (denom y))
(* (denom x) (numer y))))

(define (equal-rat? x y)
(= (* (numer x) (denom y))
(* (numer y) (denom x))))

(define (make-rat n d)
  (if ( <= (* n d) 0)
   (cons (- (abs n)) (abs d))
   (cons (abs n) (abs d))
  )
)
(define (numer x) (let ((g (gcd (car x) (cdr x)))) (/ (car x) g)))
(define (denom x) (let ((g (gcd (car x) (cdr x)))) (/ (cdr x) g)))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x))
)

;Упражнение 2.2.
;Рассмотрим задачу представления отрезков прямой на плоскости. Каждый отрезок представляется
;как пара точек: начало и конец. Определите конструктор make-segment и селекторы start-
;segment и end-segment, которые определяют представление отрезков в терминах точек. Далее,
;точку можно представить как пару чисел: координата x и координата y. Соответственно, напиши-
;те конструктор make-point и селекторы x-point и y-point, которые определяют такое пред-
;ставление. Наконец, используя свои селекторы и конструктор, напишите процедуру midpoint-
;segment, которая принимает отрезок в качестве аргумента и возвращает его середину (точку,
;координаты которой являются средним координат концов отрезка). Чтобы опробовать эти проце-
;дуры, Вам потребуется способ печатать координаты точек:

(define (make-segment start end)
  (cons start end)
)

(define (start-segment segment)
  (car segment)
)

(define (end-segment segment)
  (cdr segment)
)

(define (make-point x y)
  (cons x y)
)

(define (x-point p)
  (car p)
)

(define (y-point p)
  (cdr p)
)

(define (midpoint-segment segment)
  (let ((start (start-segment segment)) (end (end-segment segment)))
    (make-point (/ (+ (x-point start) (x-point end)) 2) (/ (+ (y-point start) (y-point end)) 2))
  )
)

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
)

(define st (make-point 1 3))
(define fn (make-point 3 5))
(define sgmnt (make-segment st fn))
(print-point (midpoint-segment sgmnt))

;Упражнение2.3.
;Реализуйте представление прямоугольников на плоскости. Определите в терминах своих конструкторов и селекторов процедуры,
;которые вычисляют периметр и площадь прямоугольника. Теперь реализуйте другое представление
;для прямоугольников. Можете ли Вы спроектировать свою систему с подходящими барьерами
;абстракции так, чтобы одни и те же процедуры вычисления периметра и площади работали с
;любым из Ваших представлений?

(define (make-poly-1 p1 p3)
  (list p1 (make-point (x-point p3) (y-point p1)) p3 (make-point (x-point p1) (y-point p3)))
)

(define (make-poly-2 p1 height width)
  (list p1 (make-point (x-point p1) ( + height (y-point p1)))(make-point ( + (x-point p1) width) ( + height (y-point p1))) (make-point ( + (x-point p1) width)  (y-point p1)))
)

(define (poly-point p num)
  (cond
    ((= num 1) (car p))
    ((= num 2) (cadr p))
    ((= num 3) (caddr p))
    (else (cadddr p))
  )
)

(define (poly-height p)
  (abs (- (y-point (poly-point p 1)) (y-point (poly-point p 3))))
)

(define (poly-width p)
  (abs (- (x-point (poly-point p 3)) (x-point (poly-point p 1))))
)

(define (poly-area p)
  (* (poly-height p) (poly-width p))
)

(define (poly-length p)
  (* 2 (+ (poly-height p) (poly-width p)))
)

(define poly-1 (make-poly-1 (make-point 1 1) (make-point 3 4)))
(define poly-2 (make-poly-2 (make-point 1 1) 2 3))
(poly-area poly-1)
(poly-area poly-2)
(poly-length poly-1)
(poly-length poly-2)