#lang racket
(define (square a)
 (* a a)
)

(define (expt b n)
 (if (= n 0)
  1
  (* b (expt b (- n 1)))
 )
)

(define (expt-linear b n)
 (define (expt-iter b counter product)
  (if (= counter 0)
   product
   (expt-iter b (- counter 1) (* b product))
  )
 )
 (expt-iter b n 1)
)

(define (fast-expt b n)
 (cond 
  ((= n 0) 1)
  ((even? n) (square (fast-expt b (/ n 2))))
  (else (* b (fast-expt b (- n 1))))
 )
)

(define (even? n)
 (= (remainder n 2) 0)
)

;Упражнение 1.16.
;Напишите процедуру, которая развивается в виде итеративного процесса и реализует возведение в
;степень за логарифмическое число шагов, как fast-expt. 
(define (fast-expt-linear b n)
 (define (expt-iter b counter product)
  (cond 
   ((= counter 0) product)
   ((even? counter) (* product (expt-iter  (square b) (quotient counter 2) 1)))
   (else (* product (expt-iter (square b) (quotient counter 2) b)))
  )
 )
 (expt-iter b n 1)
)

;Упражнение 1.17.
;Алгоритмы возведения в степень из этого раздела основаны на повторяющемся умножении. Подоб-
;ным же образом можно производить умножение с помощью повторяющегося сложения. Следующая
;процедура умножения (в которой предполагается, что наш язык способен только складывать, но
;не умножать) аналогична процедуре expt:
(define (mult a b)
 (if (= b 0)
  0
  (+ a (mult a (- b 1)))
 )
)
;Этот алгоритм затрачивает количество шагов, линейно пропорциональное b. Предположим теперь,
;что, наряду со сложением, у нас есть операции double, которая удваивает целое число, и halve,
;которая делит (четное) число на 2. Используя их, напишите процедуру, аналогичную fast-expt,
;которая затрачивает логарифмическое число шагов.

(define (double a)
 (* 2 a)
)

(define (halve a)
 (quotient a 2)
)

(define (fast-mult a b)
 (cond 
  ((= b 0) 0)
  ((even? b) (fast-mult (double a) (halve b)))
  (else (+ a (fast-mult (double a) (halve b))))
 )
)

;Упражнение 1.18.
;Используя результаты упражнений 1.16 и 1.17, разработайте процедуру, которая порождает итера-
;тивный процесс для умножения двух чисел с помощью сложения, удвоения и деления пополам, и
;затрачивает логарифмическое число шагов

(define (fast-mult-linear b n)
 (define (mult-iter b counter product)
  (cond 
   ((= counter 0) product)
   ((even? counter) (+ product (mult-iter  (double b) (halve counter) 0)))
   (else (+ product (mult-iter (double b) (halve counter) b)))
  )
 )
 (mult-iter b n 0)
)

;Упражнение 1.19.
;Существует хитрый алгоритм получения чисел Фибоначчи за логарифмическое число шагов.
;Вспомните трансформацию переменных состояния a и b процесса fib-iter из раздела 1.2.2:
;a ← a + b и b ← a. Назовем эту трансформацию T и заметим, что n-кратное применение T, начи-
;ная с 1 и 0, дает нам пару Fib(n + 1) и Fib(n). Другими словами, числа Фибоначчи получаются
;путем применения Tn, n-ой степени трансформации T, к паре (1,0). Теперь рассмотрим T как
;частный случай p = 0, q = 1 в семействе трансформаций Tpq, где Tpq преобразует пару (a, b) по
;правилу a ← bq + aq + ap, b ← bp + aq. Покажите, что двукратное применение трансформации
;Tpq равносильно однократному применению трансформации Tp′q′ того же типа, и вычислите p′ и
;q′ через p и q. Это дает нам прямой способ возводить такие трансформации в квадрат, и таким
;образом, мы можем вычислить Tn с помощью последовательного возведения в квадрат, как в
;процедуре fast-expt. Используя все эти идеи, завершите следующую процедуру, которая дает
;результат за логарифмическое число шагов:

(define (fib n)
 (define (fib-iter a b p q count)
  (cond
   ((= count 0) b)
   ((even? count) (fib-iter a b (+ (square p ) (square q)) (+ (square q) (* 2 p q)) (/ count 2)))
   (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- count 1)))
  )
 )
 (fib-iter 1 0 0 1 n)
)
