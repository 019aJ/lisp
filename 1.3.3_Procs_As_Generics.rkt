#lang racket
(define (average x y)
(/(+ x y) 2)
  )
(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        midpoint
        (let ((test-value (f midpoint)))
          (cond ((positive? test-value)
                 (search f neg-point midpoint))
                ((negative? test-value)
                 (search f midpoint pos-point))
                (else midpoint)))))
)
(define (close-enough? x y)(< (abs (- x y)) 0.001))

(define (half-interval-method f a b)
(let ((a-value (f a))
(b-value (f b)))
(cond ((and (negative? a-value) (positive? b-value))
(search f a b))
((and (negative? b-value) (positive? a-value))
(search f b a))
(else
(error "У аргументов не разные знаки " a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))1.0 2.0)

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess)
)

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (sqrt x)(fixed-point (lambda (y) (average y (/ x y))) 1.0))

;Упражнение 1.35.
;Покажите, что золотое сечение  есть неподвижная точка трансформации x → 1 + 1/x,
;и используйте этот факт для вычисления ЗС с помощью процедуры fixed-point.
(fixed-point (lambda (y) (+ (/ 1 y) 1)) 2.0)
;Упражнение 1.36.
;Измените процедуру fixed-point так, чтобы она печатала последовательность приближений,
;которые порождает, с помощью примитивов newline и display, показанных в упражнении 1.22.
;Затем найдите решение уравнения x^x = 1000 путем поиска неподвижной точки
;x → log(1000)/ log(x). Посчитайте, сколько шагов это занимает при использовании торможения
;усреднением и без него. (Учтите, что нельзя начинать fixed-point со значения 1, поскольку это
;вызовет деление на log(1) = 0.)
(define (fixed-point-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    
    (let ((next (f guess)))
      (display "guess: ")(display guess)(display " next: ")(display next)(newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess)
  (display "finish ")(newline)
)
(fixed-point-display (lambda (y) (+ (/ 1 y) 1)) 2.0)
(fixed-point-display (lambda (y) (/ (log 1000) (log y))) 2.0)

Упражнение 1.37.
;а. Определите процедуру cont-frac так, чтобы вычисление (cont-frac n d k) давало значение k-элементной конечной цепной дроби. 