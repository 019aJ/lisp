#lang racket
(require  "1.2.6_Primes.rkt")
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
 (start-prime-test n (runtime))
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
(define (smallest-divisor-2 n)
 (define (next x)
  (if (= x 2) 3
   (+ x 2)
  )
 )
 (define (find-divisor n test-divisor)
  (cond
   ((> (square test-divisor) n) n)
   ((divides? test-divisor n) test-divisor)
   (else (find-divisor n (next test-divisor)))
  )
 )
 (find-divisor n 2)
)

;Упражнение 1.24.
;Модифицируйте процедуру timed-prime-test из упражнения 1.22 так, чтобы она использовала
;fast-prime? (метод Ферма) и проверьте каждое из 12 простых чисел, найденных в этом упраж-
;нении.

(define (timed-prime-test-fast n)
 (define (start-prime-test n start-time)
  (if (fast-prime? n 1000)
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
 (start-prime-test n (runtime))
)
(define (search-for-primes-fast left-bound right-bound)
  (cond
    ((> left-bound right-bound) (newline) (display "Вычисление окончено"))
    ((even? left-bound) (search-for-primes-fast (+ left-bound 1) right-bound))
    (else (timed-prime-test-fast left-bound) (search-for-primes-fast (+ left-bound 1) right-bound))
  )
)
(search-for-primes-fast 1000 1019)
(newline)
(search-for-primes-fast 10000 10037)
(newline)
(search-for-primes-fast 100000 100043)
(newline)
(search-for-primes-fast 1000000 1000037)
(newline)
(search-for-primes-fast 10000000 10000103)
(newline)
(search-for-primes-fast 100000000 100000039)
(newline)
;Упражнение 1.27.
;Покажите, что числа Кармайкла (561, 1105, 1729, 2465, 2821 и 6601) действительно «обманывают» тест
;Ферма: напишите процедуру, которая берет целое число n и проверяет, правда ли a^n = a
;по модулю n для всех a < n, и проверьте эту процедуру на этих числах Кармайкла.

;Остаток от деления числа a на n называется также остатком a по модулю n (remainder of a modulo n) или просто a по модулю n.
(define (karmikle n)
 (define (inner-check a)
  (cond
    ((>= a n) #t)
    ((= a (expmod a n n))(inner-check (+ a 1)))
    (else #f)
  )
 )
 (inner-check 2)
)
(display "числа Кармайкла и тест Ферма")(newline)
(display "561: ")(display (karmikle 561))(newline)
(display "1105: ")(display (karmikle 1105))(newline)
(display "1729: ")(display (karmikle 1729))(newline)
(display "2465: ")(display (karmikle 2465))(newline)
(display "2821: ")(display (karmikle 2821))(newline)
(display "6601: ")(display (karmikle 6601))(newline)

;Упражнение 1.28.
;Один из вариантов теста Ферма, который невозможно обмануть, называется тест Миллера–Ра-
;бина (Miller-Rabin test). Он основан на альтернативной формулировке
;Малой теоремы Ферма, которая состоит в том, что если n — простое число, а a — произвольное
;положительное целое число, меньшее n, то a в n - 1-ой степени равняется 1 по модулю n. Про-
;веряя простоту числа n методом Миллера–Рабина, мы берем случайное число a < n и возводим
;его в (n − 1)-ю степень по модулю n с помощью процедуры expmod. Однако когда в процеду-
;ре expmod мы проводим возведение в квадрат, мы проверяем, не нашли ли мы «нетривиальный
;квадратный корень из 1 по модулю n», то есть число, не равное 1 или n − 1, квадрат которого
;по модулю n равен 1.; Модифицируйте процедуру expmod так, чтобы она
;сигнализировала обнаружение нетривиального квадратного корня из 1, и используйте ее для ре-
;ализации теста Миллера–Рабина с помощью процедуры, аналогичной fermat-test. Проверьте
;свою процедуру на нескольких известных Вам простых и составных числах. Подсказка: удобный
;способ заставить expmod подавать особый сигнал — заставить ее возвращать 0.

(define (expmod-miller-rabin base exp m)
 (cond
  ((= exp 0) 1)
  ((even? exp)
    (let ((value (expmod-miller-rabin base (/ exp 2) m)))
      (if (and (not (= 1 value)) (not (= (- m 1) value)) (= 1 (remainder  (square value) m)))
       0
       (remainder (square value) m)
      )   
    )
  )
  (else (remainder (* base (expmod-miller-rabin base (- exp 1) m)) m))
 )
)

(define (miller-rabin-test n)
 (define (try-it a)
  (= (expmod-miller-rabin a n n) a)
 )
 (let ((check (random (- n 1))))
  (try-it (+ 1 check))
 )
)

(define (miller-rabin-prime? n times)
 (cond
  ((= times 0) #t)
  ((miller-rabin-test n) (miller-rabin-prime? n (- times 1)))
  (else #f)
 )
)
(display "числа Кармайкла и тест Миллера–Рабина")(newline)
(display "561: ")(display (miller-rabin-prime? 561 500))(newline)
(display "1105: ")(display (miller-rabin-prime? 1105 500))(newline)
(display "1729: ")(display (miller-rabin-prime? 1729 500))(newline)
(display "2465: ")(display (miller-rabin-prime? 2465 500))(newline)
(display "2821: ")(display (miller-rabin-prime? 2821 500))(newline)
(display "6601: ")(display (miller-rabin-prime? 6601 500))(newline)
(provide prime?)