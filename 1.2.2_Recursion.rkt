#lang racket
(define (fib n)
 (cond 
  ((= n 0) 0)
  ((= n 1) 1)
  (else (+ (fib (- n 1)) (fib (- n 2))))
 )
)

(define (count-change amount)
 (cc amount 5)
)
(define (cc amount kinds-of-coins)
 (cond 
  ((= amount 0) 1)
  ((or (< amount 0) (= kinds-of-coins 0)) 0)
  (else (+ (cc amount(- kinds-of-coins 1))(cc (- amount (first-denomination kinds-of-coins)) kinds-of-coins)))
 )
)
(define (first-denomination kinds-of-coins)
 (cond 
  ((= kinds-of-coins 1) 1)
  ((= kinds-of-coins 2) 5)
  ((= kinds-of-coins 3) 10)
  ((= kinds-of-coins 4) 25)
  ((= kinds-of-coins 5) 50)
 )
)

;Упражнение 1.11.
;Функция f определяется правилом: f(n) = n, если n < 3, и f(n) = f(n-1)+f(n-2)+f(n-3),
;если n > 3. Напишите процедуру, вычисляющую f с помощью рекурсивного процесса. Напишите
;процедуру, вычисляющую f с помощью итеративного процесса.

(define (f-recursive n) 
 (cond 
  ((= n 0) 0)
  ((= n 1) 1)
  ((= n 2) 2)
  (else (+ (f-recursive (- n 1)) (f-recursive (- n 2)) (f-recursive (- n 3))))
 )
)

(define (f-iterative n) 
 (define (f-inner f0 f1 f2 i)
  (if (< i n)
   (f-inner f1 f2 (+ f0 f1 f2) (+ i 1))
   f2
  )
 )
 (cond 
  ((= n 0) 0)
  ((= n 1) 1)
  ((= n 2) 2)
  (else (f-inner 0 1 2 2))
 )
)
(display "f-recursive for 10 =")(display(f-recursive 10))(newline)
(display "f-iterative for 10 =")(display(f-iterative 10))(newline)

;Упражнение 1.12.
;Напишите процедуру, вычисляющую элементы треугольника Паскаля с помощью рекурсивного процесса.
(define (pascal-triangle n)
 (define (pascal-num row column)
  (cond
   ((= row 1) 1)
   ((= column 1) 1)
   ((= column row) 1)
   (else (+ (pascal-num (- row 1) (- column 1))(pascal-num (- row 1) column)))
  )
 )
 (define (pascal-row j) 
  (cond 
   ((<= j n)
    (display (pascal-num n j))
    (display " ")
    (pascal-row (+ j 1)))
  )
 )
 (pascal-row 1) 
)

