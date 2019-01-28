#lang racket
(require racket/include)
(require  "1.2.6_Primes.rkt")
(define (cube x) (* x x x))
(define (sum term a next b)
 (if (> a b)
  0
  (+ (term a)(sum term (next a) next b))
 )
)
(define (inc n) (+ n 1))
(define (sum-cubes a b)(sum cube a inc b))
(display "Сумма кубов от 1 до 10: ")(display(sum-cubes 1 10))(newline)
(define (identity x) x)
(define (sum-integers a b)(sum identity a inc b))
(display "Сумма чисел от 1 до 10: ")(display(sum-integers 1 10))(newline)

(define (integral f a b dx)
 (define (add-dx x) (+ x dx))
 (* (sum f (+ a (/ dx 2)) add-dx b)dx)
)
(display "Интеграл x^3  от 0 до 1 c шагом 0.01: ")(display (integral cube 0 1 0.01))(newline)
(display "Интеграл x^3  от 0 до 1 c шагом 0.001: ")(display (integral cube 0 1 0.001))(newline)
;Упражнение 1.29.
;Правило Симпсона — более точный метод численного интегрирования, чем представленный выше.
;С помощью правила Симпсона интеграл функции f между a и b приближенно вычисляется в виде
;h/3 * [y0 + 4y1 + 2y2 + 4y3 + 2y4 + . . . + 2yn-2 + 4yn-1 + yn]
;где h = (b − a)/n, для какого-то четного целого числа n, а yk = f(a + kh). Определите процедуру, которая принимает в качестве
;аргументов f, a, b и n, и возвращает значение интеграла, вычисленное по правилу Симпсона. С
;помощью этой процедуры проинтегрируйте cube между 0 и 1 (с n = 100 и n = 1000) и сравните
;результаты с процедурой integral, приведенной выше.
(define (integral-simpson f a b n)
 (define h (/ (- b a) n))
 (define (yk k) (f (+ a (* k h))))
 (define (koef k)
   (cond
     ((= 0 k) 1)
     ((= n k) 1)
     ((even? k) 4)
     (else 2)
   )
 )
 (define (sum-term i)
   (* (koef i)(yk i))
 )
 ( + (* (/ h 3) (sum sum-term 0 inc n)) 0.0)
)
(display "Интеграл x^3  от 0 до 1 c шагом 0.01: ")(display (integral-simpson cube 0 1 100))(newline)
(display "Интеграл x^3  от 0 до 1 c шагом 0.001: ")(display (integral-simpson cube 0 1 1000))(newline)

;Упражнение 1.30.
;Процедура sum порождает линейную рекурсию. Ее можно переписать так, чтобы суммирование
;выполнялось итеративно. Покажите, как сделать это, заполнив пропущенные выражения в следу-
;ющем определении:
(define (sum-iter term a next b)
 (define (iter a result)
  (if (> a b)
   result
   (iter (next a) (+ (term a) result))
  )
 )
 (iter a 0)
)
(define (sum-cubes-iter a b)(sum-iter cube a inc b))
(display "Сумма кубов от 1 до 10 итеративно: ")(display(sum-cubes-iter 1 10))(newline)

;Упражнение 1.31.
;а. Напишите процедуру под названием product, которая вычисляет произведение значений функции в точках на указан-
;ном интервале.
(define (product term a next b)
 (if (> a b)
  1
  (* (term a)(product term (next a) next b))
 )
)
(define (koef x)
 (if (even? x)
  ( / (+ x 2) (+ x 1))
  ( / (+ x 1) (+ x 2))
 )
)
(define (pi-yollis n)
  ( + (* (product koef 1 inc n) 4) 0.0)
)

(display "pi = ")(display(pi-yollis 1000))(newline)
;б. Если Ваша процедура product порождает рекурсивный процесс, перепишите ее так, чтобы
;она порождала итеративный. Если она порождает итеративный процесс, перепишите ее так, чтобы
;она порождала рекурсивный
(define (product-iter term a next b)
 (define (iter a result)
  (if (> a b)
   result
   (iter (next a) (* (term a) result))
  )
 )
 (iter a 1)
)
(define (pi-yollis-iter n)
  ( + (* (product-iter koef 1 inc n) 4) 0.0)
)

(display "pi итеративно = ")(display(pi-yollis-iter 1000))(newline)

;Упражнение 1.32.
;а. Accumulate принимает в качестве аргументов те же описания термов и диапазона, что и sum с
;product, а еще процедуру combiner (двух аргументов), которая указывает, как нужно присо-
;единить текущий терм к результату накопления предыдущих, и null-value, базовое значение,
;которое нужно использовать, когда термы закончатся. Напишите accumulate и покажите, как и
;sum, и product можно определить в виде простых вызовов accumulate.
(define (accumulate combiner null-value term a next b)
 (if (> a b)
   null-value
  (combiner (term a) (accumulate combiner  null-value term (next a) next b))
 )
)
(define (sum-acc term a next b)
 (accumulate + 0 term a next b)
)

(define (product-acc term a next b)
 (accumulate * 1 term a next b)
)

(define (sum-cubes-acc a b)(sum-acc cube a inc b))
(display "Сумма кубов от 1 до 10 через accumulate ")(display(sum-cubes-acc 1 10))(newline)
(define (pi-yollis-acc n)
  ( + (* (product-acc koef 1 inc n) 4) 0.0)
)
(display "pi  через accumulate  = ")(display(pi-yollis-acc 1000))(newline)

;б. Если Ваша процедура accumulate порождает рекурсивный процесс, перепишите ее так,
;чтобы она порождала итеративный. Если она порождает итеративный процесс, перепишите ее так,
;чтобы она порождала рекурсивный.
(define (accumulate-iter combiner null-value term a next b)
 (define (iter a result)
  (if (> a b)
   result
   (iter (next a) (combiner (term a) result))
  )
 )
 (iter a null-value)
)

(define (sum-acc-iter term a next b)
 (accumulate-iter + 0 term a next b)
)

(define (product-acc-iter term a next b)
 (accumulate-iter * 1 term a next b)
)

(define (sum-cubes-acc-iter a b)(sum-acc-iter cube a inc b))
(display "Сумма кубов от 1 до 10 через accumulate итеративно ")(display(sum-cubes-acc-iter 1 10))(newline)
(define (pi-yollis-acc-iter n)
  ( + (* (product-acc-iter koef 1 inc n) 4) 0.0)
)
(display "pi  через accumulate итеративно = ")(display(pi-yollis-acc-iter 1000))(newline)

;Упражнение 1.33.
;Можно получить еще более общую версию accumulate (упражнение 1.32), если ввести понятие
;фильтра (filter) на комбинируемые термы. То есть комбинировать только те термы, порожденные
;из значений диапазона, которые удовлетворяют указанному условию. Получающаяся абстракция
;filtered-accumulate получает те же аргументы, что и accumulate, плюс дополнительный
;одноаргументный предикат, который определяет фильтр. Запишите filtered-accumulate в
;виде процедуры. Покажите, как с помощью filtered-accumulate выразить следующее:
;а. сумму квадратов простых чисел в интервале от a до b (в предположении, что процедура
;prime? уже написана);
;б. произведение всех положительных целых чисел меньше n, которые просты по отношению к
;n (то есть всех таких положительных целых чисел i < n, что НОД(i, n) = 1).

(define (accumulate-filter combiner null-value filter term a next b)
 (cond
  ((> a b) null-value)
  ((filter a) (combiner (term a) (accumulate-filter combiner  null-value filter term (next a) next b)))
  (else (accumulate-filter combiner  null-value filter term (next a) next b))
 )
)
(define (sum-prime-squares a b)
 (accumulate-filter + 0 prime? square a inc b)
)
(display "Сумма квадратов простых чисел от 1 до 10 ")(display(sum-prime-squares 1 10))(newline)

(provide (all-defined-out))