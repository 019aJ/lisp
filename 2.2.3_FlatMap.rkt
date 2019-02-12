#lang racket
(require racket/include)
(require  "2.2.3_List_As_Conventional_Interface.rkt")
(require  "1.2.6_Primes.rkt")
(define (flatmap proc seq)
(accumulate append '() (map proc seq)))

(define (prime-sum? pair)
(prime? (+ (car pair) (cadr pair))))

(define (make-pair-sum pair)
(list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high)))
)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n))))
)

(define (permutations s)
  (if (null? s) 
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s))
)

(define (remove item sequence)
(filter (lambda (x) (not (= x item))) sequence))

;Упражнение 2.40.
;Определите процедуру unique-pairs, которая, получая целое число n, порождает последова-
;тельность пар (i, j), таких, что 1 ≤ j < i ≤ n. С помощью unique-pairs упростите данное выше
;определение prime-sum-pairs.
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
)
(define (prime-sum-pairs-simple n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n)))
)
;Упражнение 2.41.
;Напишите процедуру, которая находит все такие упорядоченные тройки различных положительных
;целых чисел i, j и k, меньших или равных данному целому числу n, сумма которых равна данному
;числу s.
(define (unique-trio n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
             (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1))))
                  (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n))
)
(define (trio-sum n)
  (filter (lambda(l) (= n (accumulate + 0 l)))
          (unique-trio n))
)
(trio-sum 9)
;Упражнение 2.42.
;В «задаче о восьми ферзях» спрашивается, как расставить на шахматной доске восемь ферзей так,
;чтобы ни один из них не бил другого (то есть никакие два ферзя не должны находиться на одной
;вертикали, горизонтали или диагонали). Одно из возможных решений показано на рисунке 2.8.
;Один из способов решать эту задачу состоит в том, чтобы идти поперек доски, устанавливая по
;ферзю в каждой вертикали. После того, как k - 1 ферзя мы уже разместили, нужно разместить
;k-го в таком месте, где он не бьет ни одного из тех, которые уже находятся на доске. Этот под-
;ход можно сформулировать рекурсивно: предположим, что мы уже породили последовательность
;из всех возможных способов разместить k − 1 ферзей на первых k − 1 вертикалях доски. Для
;каждого из этих способов мы порождаем расширенный набор позиций, добавляя ферзя на каж-
;дую горизонталь k-й вертикали. Затем эти позиции нужно отфильтровать, оставляя только те, где
;ферзь на k-й вертикали не бьется ни одним из остальных. Продолжая этот процесс, мы породим
;не просто одно решение, а все решения этой задачи.
;Это решение мы реализуем в процедуре queens, которая возвращает последовательность ре-
;шений задачи размещения n ферзей на доске n × n. В процедуре queens есть внутренняя проце-
;дура queen-cols, которая возвращает последовательность всех способов разместить ферзей на
;первых k вертикалях доски.
(define (queens board-size)
  (define empty-board
    '()
  )
  (define (get-index row) (car (filter (lambda (x) (pair? x)) row)) )
  
  (define (check k pos)
    (if
     (= k 1) #t
     (let ((index (get-index (car pos))))
       (empty?
        (filter (lambda (x)
                  (let ((row-index (get-index x)))
                    (cond ((= (car row-index) (car index)) #t)
                          ((= (car row-index) ( + (car index) (- (cadr index) (cadr row-index)))) #t)
                          ((= (car row-index) ( - (car index) (- (cadr index) (cadr row-index)))) #t)
                          (else #f))
                    ))
                (cdr pos))))
     )   
  )
  
  (define (safe? k pos) (if (= k 1) #t (check k pos)))
  
  (define (adjoin-position new-row k rest-of-queens) (append (list (flatmap (lambda (y) (list (if (= y new-row) (list new-row k) 0))) (enumerate-interval 1 board-size))) rest-of-queens))
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))

  (map (lambda (z) (map (lambda (x) (map (lambda (y) (if (pair? y) 1 y)) x)) z)) (queen-cols board-size))
)
(queens 4)

;В этой процедуре rest-of-queens есть способ размещения k - 1 ферзя на первых k - 1 верти-
;калях, а new-row — это горизонталь, на которую предлагается поместить ферзя с k-й вертикали.

;Завершите эту программу, реализовав представление множеств позиций ферзей на доске, включая
;процедуру adjoin-position, которая добавляет нового ферзя на определенных горизонтали и
;вертикали к заданному множеству позиций, и empty-board, которая представляет пустое множе-
;ство позиций. Еще нужно написать процедуру safe?, которая для множества позиций определяет,
;находится ли ферзь с k-й вертикали в безопасности от остальных. (Заметим, что нам требуется
;проверять только то, находится ли в безопасности новый ферзь — для остальных ферзей безопас-
;ность друг от друга уже гарантирована.)

;Упражнение 2.43.;
;У Хьюго Дума ужасные трудности при решении упражнения 2.42. Его процедура queens вроде
;бы работает, но невероятно медленно. (Хьюго ни разу не удается дождаться, пока она решит хотя
;бы задачу 6 × 6.) Когда Хьюго просит о помощи Еву Лу Атор, она указывает, что он поменял
;местами порядок вложенных отображений в вызове процедуры flatmap, записав его в виде
;(flatmap
;(lambda (new-row)
;(map (lambda (rest-of-queens)
;(adjoin-position new-row k rest-of-queens))
;(queen-cols (- k 1))))
;(enumerate-interval 1 board-size))
;Объясните, почему из-за такой перестановки программа работает медленно. Оцените, насколько
;долго программа Хьюго будет решать задачу с восемью ферзями, если предположить, что про-
;грамма, приведенная в упражнении 2.42, решает ее за время T.