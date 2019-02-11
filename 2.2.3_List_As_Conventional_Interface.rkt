#lang racket
(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence))))
)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence))))
)

(define (enumerate-tree tree)
   (cond ((null? tree) null)
         ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                       (enumerate-tree (cdr tree))))))

;Упражнение 2.33.
;Заполните пропущенные выражения, так, чтобы получились определения некоторых базовых опе-
;раций по работе со списками в виде накопления:
(define (map-a p sequence)(accumulate (lambda (x y) (append (list (p x)) y)) '() sequence))
(define (append-a seq1 seq2)(accumulate cons seq2 seq1))
(define (length-a sequence)(accumulate (lambda (x y) (+ y 1)) 0 sequence))

;Упражнение 2.34.
;Вычисление многочлена с переменной x при данном значении x можно сформулировать в виде
;накопления. Мы вычисляем многочлен anxn + an-1xn-1 + . . . + a1x + a0
;по известному алгоритму, называемому схема Горнера (Horner’s rule), которое переписывает
;формулу в виде
;(. . . (anx + an−1)x + . . . + a1)x + a0)
;Другими словами, мы начинаем с an, умножаем его на x, и так далее, пока не достигнем a0
;Заполните пропуски в следующей заготовке так, чтобы получить процедуру, которая вычисляет
;многочлены по схеме Горнера. Предполагается, что коэффициенты многочлена представлены в
;виде последовательности, от a0 до an.
(define (horner-eval x coefficient-sequence)
(accumulate (lambda (this-coeff higher-terms) (+ this-coeff(* x higher-terms))) 0 coefficient-sequence))
;Например, чтобы вычислить 1 + 3x + 5x3 + x5 в точке x = 2, нужно ввести
(horner-eval 2 (list 1 3 0 5 0 1))

;Упражнение 2.35.
;Переопределите count-leaves из раздела 2.2.2 в виде накопления:
(define (count-leaves t)
(accumulate (lambda (x y) (+ x y)) 0 (map (lambda (z) 1)(enumerate-tree t))))

;Упражнение 2.36.
;Процедура accumulate-n подобна accumulate, только свой третий аргумент она восприни-
;мает как последовательность последовательностей, причем предполагается, что все они содержат
;одинаковое количество элементов. Она применяет указанную процедуру накопления ко всем пер-
;вым элементам последовательностей, вторым элементам последовательностей и так далее, и воз-
;вращает последовательность результатов. Например, если s есть последовательность, состоящая
;из четырех последовательностей, ((1 2 3) (4 5 6) (7 8 9) (10 11 12)), то значением
;(accumulate-n + 0 s) будет последовательность (22 26 30). Заполните пробелы в следую-
;щем определении accumulate-n:
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (accumulate (lambda (x y) (append (list (car x)) y)) '() seqs))
            (accumulate-n op init (accumulate  (lambda (x y) (append (list (cdr x)) y)) '() seqs))))
)
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)) )
(accumulate-n + 0 s)

;Упражнение 2.37.
;Предположим, что мы представляем векторы v = (vi) как последовательности чисел, а матрицы
;m = (mij) как последовательности векторов (рядов матрицы). Например, матрица
;представляется в виде последовательности ((1 2 3 4) (4 5 6 6) (6 7 8 9)). Имея такое
;представление, мы можем использовать операции над последовательностями, чтобы кратко выра-
;зить основные действия над матрицами и векторами:
;Скалярное произведение
(define (dot-product v w)
(accumulate + 0 (map * v w)))
;Произведение матрицы и вектора
(define (matrix-*-vector m v)
(map (lambda (x) (dot-product x v)) m))
;Транспозиция
(define (transpose mat)
(accumulate-n (lambda (x y) (append (list x) y)) '() mat))
;Произведение матриц (matrix-*-matrix m n) возвращает матрицу p, где
(define (matrix-*-matrix m n)
(let ((cols (transpose n)))
(map (lambda (x) (matrix-*-vector cols x)) m)))

(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
(define n (list (list 2 3 2) (list 1 4 2) (list 4 5 3) (list 5 6 1)))
(define v (list 1 1 1 1))
(dot-product v v)
(matrix-*-vector m v)
(transpose m)
(matrix-*-matrix m n)

;Упражнение 2.38.
;Процедура accumulate известна также как fold-right (правая свертка), поскольку она комби-
;нирует первый элемент последовательности с результатом комбинирования всех элементов справа
;от него. Существует также процедура fold-left (левая свертка), которая подобна fold-right,
;но комбинирует элементы в противоположном направлении:
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence)
)
(define (fold-right  op initial sequence) (accumulate op initial sequence))
;Каковы значения следующих выражений?
(fold-right / 1 (list 1 2 3))
(fold-left / 1 (list 1 2 3))
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))
;Укажите свойство, которому должна удовлетворять op, чтобы для любой последовательности
;fold-right и fold-left давали одинаковые результаты.
;коммутативность

;Упражнение 2.39.
;Закончите следующие определения reverse (упражнение 2.18) в терминах процедур fold-
;right и fold-left из упражнения 2.38.
(define (reverse sequence)
(fold-right (lambda (x y) (append y (list x))) '() sequence))
(define (reverse-2 sequence)
(fold-left (lambda (x y) (append (list y) x)) '() sequence))
