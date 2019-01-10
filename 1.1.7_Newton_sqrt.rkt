#lang racket
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x))
)
(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x y) 2)
)
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.0001)
)
(define (square a)(* a a))

;Упражнение 1.6.
;Лиза П. Хакер не понимает, почему if должна быть особой формой. «Почему нельзя просто
;определить ее как обычную процедуру с помощью cond?» — спрашивает она. Лизина подруга Ева
;Лу Атор утверждает, что, разумеется, можно, и определяет новую версию if:
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause))
  )

;Обрадованная Лиза переписывает через new-if программу вычисления квадратного корня:
(define (sqrt-iter-new guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new (improve guess x) x))
)
;Что получится, когда Лиза попытается использовать эту процедуру для вычисления квадратных
;корней? Объясните.
;Решение
;Произойдет зацикливание, т.к. для обычных функций, в отличии от специальных форм, сначала вычисляются значения операндов. Т.е. 
;(new-if (good-enough? guess x) guess (sqrt-iter-new (improve guess x) x)) попытается вычислить (sqrt-iter-new (improve guess x) x), которая
;снова попытается вычислить new-if и т.д.

;Упражнение 1.7.
;Проверка good-enough?, которую мы использовали для вычисления квадратных корней, будет
;довольно неэффективна для поиска квадратных корней от очень маленьких чисел. Кроме того, в
;настоящих компьютерах арифметические операции почти всегда вычисляются с ограниченной точ-
;ностью. Поэтому наш тест оказывается неадекватным и для очень больших чисел. Альтернативный
;подход к реализации good-enough? состоит в том, чтобы следить, как от одной итерации к дру-
;гой изменяется guess, и остановиться, когда изменение оказывается небольшой долей значения
;приближения. Разработайте процедуру вычисления квадратного корня, которая использует такой
;вариант проверки на завершение. Верно ли, что на больших и маленьких числах она работает
;лучше?

(define (good-enough-guess? guess prev-guess)
  (< (abs (- guess prev-guess)) 0.0001)
)
(define (sqrt-iter-1-7 guess x)
  (define (sqrt-iter-inner guess prev-guess x)
    (if (good-enough-guess? guess prev-guess)
      guess
      (sqrt-iter-inner (improve guess x) guess x)
     )
  )
  (sqrt-iter-inner guess (+ guess 1) x)
)
(display "sqrt 0.00000004 (= 0.0002)")(newline)
(sqrt-iter-1-7 1 0.00000004)
(sqrt-iter 1 0.00000004)
(newline)
(display "sqrt 0.0000000256 (= 0.00016)")(newline)
(sqrt-iter-1-7 1 0.0000000256)
(sqrt-iter 1 0.0000000256)
(newline)
(display "sqrt 0.000004 (= 0.002)")(newline)
(sqrt-iter-1-7 1 0.000004)
(sqrt-iter 1 0.000004)

;Упражнение 1.8.
;Метод Ньютона для кубических корней основан на том, что если y является приближением к
;кубическому корню из x, то мы можем получить лучшее приближение по формуле (x/y2 + 2y)/3
;С помощью этой формулы напишите процедуру вычисления кубического корня, подобную проце-
;дуре для квадратного корня.

(define (improve-cube guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)
)

(define (cube-sqrt-iter guess x)
  (define (sqrt-iter-inner guess prev-guess x)
    (if (good-enough-guess? guess prev-guess)
      guess
      (sqrt-iter-inner (improve-cube guess x) guess x)
     )
  )
  (sqrt-iter-inner guess (+ guess 1) x)
)
(display "8^1/3 = 2")(newline)
(cube-sqrt-iter 1 8)
(newline)
(display "27^1/3 = 3")(newline)
(cube-sqrt-iter 1 27)
(newline)