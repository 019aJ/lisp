#lang racket
(define (runtime) (current-milliseconds))

(define (square a)
 (* a a)
)

(define (smallest-divisor n)
 (find-divisor n 2)
)
(define (find-divisor n test-divisor)
 (cond
  ((> (square test-divisor) n) n)
  ((divides? test-divisor n) test-divisor)
  (else (find-divisor n (+ test-divisor 1)))
 )
)
(define (divides? a b)
 (= (remainder b a) 0)
)

(define (prime? n)
 (= n (smallest-divisor n))
)
;Тест Ферма
(define (expmod base exp m)
 (cond
  ((= exp 0) 1)
  ((even? exp)
  (remainder (square (expmod base (/ exp 2) m)) m))
  (else (remainder (* base (expmod base (- exp 1) m)) m))
 )
)

(define (fermat-test n)
 (define (try-it a)
  (= (expmod a n n) a)
 )
 (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
 (cond
  ((= times 0) #t)
  ((fermat-test n) (fast-prime? n (- times 1)))
  (else #f)
 )
)

;Упражнение 1.21.
;С помощью процедуры smallest-divisor найдите наименьший делитель следующих чисел:
;199, 1999, 19999.
(display "наименьший делитель 199 ")(display (smallest-divisor 199))(newline)
(display "наименьший делитель 1999 ")(display (smallest-divisor 1999))(newline)
(display "наименьший делитель 19999 ")(display (smallest-divisor 19999))(newline)

;Упражнение 1.22.
;Большая часть реализаций Лиспа содержат элементарную процедуру runtime, которая воз-
;вращает целое число, показывающее, как долго работала система (например, в миллисекундах).
;Следующая процедура timed-prime-test, будучи вызвана с целым числом n, печатает n и про-
;веряет, простое ли оно. Если n простое, процедура печатает три звездочки и количество времени,
;затраченное на проверку.
(define (timed-prime-test n)
 (start-prime-test n (runtime))
)
(define (start-prime-test n start-time)
 (if (prime? n)
  (report-prime n (- (runtime) start-time))
  #f
 )
)
(define (report-prime val elapsed-time)
 (display val)
 (display " *** ")
 (display elapsed-time)
 (newline)
)
;Используя эту процедуру, напишите процедуру search-for-primes, которая проверяет на про-
;стоту все нечетные числа в заданном диапазоне

(define (search-for-primes left-bound right-bound)
  (cond
    ((> left-bound right-bound) (newline) (display "Вычисление окончено"))
    ((even? left-bound) (search-for-primes (+ left-bound 1) right-bound))
    (else (timed-prime-test left-bound) (search-for-primes (+ left-bound 1) right-bound))
  )
)

(search-for-primes 1000 1019)
(newline)
(search-for-primes 10000 10037)
(newline)
(search-for-primes 100000 100043)
(newline)
(search-for-primes 1000000 1000037)
(newline)
(search-for-primes 10000000 10000103)
(newline)
(search-for-primes 100000000 100000039)

;Упражнение 1.23.
;Процедура smallest-divisor в начале этого раздела проводит множество лишних проверок:
;после того, как она проверяет, делится ли число на 2, нет никакого смысла проверять делимость
;на другие четные числа. Таким образом, вместо последовательности 2, 3, 4, 5, 6 . . . , используе-
;мой для test-divisor, было бы лучше использовать 2, 3, 5, 7, 9 . . . . Чтобы реализовать такое
;улучшение, напишите процедуру next, которая имеет результатом 3, если получает 2 как аргу-
;мент, а иначе возвращает свой аргумент плюс 2. Используйте (next test-divisor) вместо (+
;test-divisor 1) в smallest-divisor. 