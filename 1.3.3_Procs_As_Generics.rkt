#lang racket
(require racket/include)
(require  "1.2.4_Power.rkt")
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

;Упражнение 1.37.
;а. Определите процедуру cont-frac так, чтобы вычисление (cont-frac n d k) давало значение k-элементной конечной цепной дроби.
(define (cont-frac n d k)
 (define (inner prev-val index)
   (if (> 1 index)
     prev-val
     (inner (/ (n index) (+ (d index) prev-val)) (- index 1))
   )    
 )
 (inner 0 k)
)

(cont-frac (lambda (i) 1.0) (lambda (i) 1.0) 11)


;Если Ваша процедура cont-frac порождает рекурсивный процесс, напишите вариант, кото-
;рый порождает итеративный процесс. Если она порождает итеративный процесс, напишите вари-
;ант, порождающий рекурсивный процесс.

(define (cont-frac-iter n d k)
 (define (inner index)
   (if (> index k)
     0
     (/ (n index)  (+ (d index) (inner (+ index 1))))
   )    
 )
 (inner 1)
)

(cont-frac-iter (lambda (i) 1.0) (lambda (i) 1.0) 11)

;Упражнение 1.38.
;В 1737 году швейцарский математик Леонард Эйлер опубликовал статью De functionibus
;Continuis, которая содержала расширение цепной дроби для e-2, где e — основание натуральных
;логарифмов. В этой дроби все Ni равны 1, а Di последовательно равны 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, . . .Напишите
;программу, использующую Вашу процедуру cont-frac из упражнения 1.37 для вычисления e на основании формулы Эйлера.
( + 2 (cont-frac (lambda (i) 1.0)
           (lambda (i)
             (if (= 2 (remainder i 3))
                 (* 2 (+ (quotient i 3) 1))
                 1
             )
           )
           11))


;Упражнение 1.39.
;Представление тангенса в виде цепной дроби было опубликовано в 1770 году немецким математи-
;ком Й.Х. Ламбертом. Определите процедуру (tan-cf x k), которая вычисляет приближение к
;тангенсу на основе формулы Ламберта. K указывает количество термов, которые требуется вычис-
;лить, как в упражнении 1.37.
(define (tan-cf x k)
  ( + (cont-frac (lambda (i) (if (= i 1) x (- 0 (square x)))) (lambda (i) (- (* 2 i) 1) ) k) 0.0)
)
(tan-cf 1 10)
(provide (all-defined-out))