#lang racket
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items))))
)

;Упражнение 2.21.
;Процедура square-list принимает в качестве аргумента список чисел и возвращает список
;квадратов этих чисел.
;Перед Вами два различных определения square-list. Закончите их, вставив пропущенные вы-
;ражения:
(define (square x) (* x  x))
(define (square-list-1 items)
  (if (null? items)
      '()
      (cons (square (car items)) (square-list-1 (cdr items))))
)

(define (square-list items)
(map square items))

;Упражнение 2.23.
;Процедура for-each похожа на map. В качестве аргументов она принимает процедуру и спи-
;сок элементов. Однако вместо того, чтобы формировать список результатов, for-each просто
;применяет процедуру по очереди ко всем элементам слева направо. Результаты применения про-
;цедуры к аргументам не используются вообще — for-each применяют к процедурам, которые
;осуществляют какое-либо действие вроде печати. 
;Значение, возвращаемое вызовом for-each (оно в листинге не показано) может быть каким
;угодно, например истина. Напишите реализацию for-each.

(define (for-each proc items)
 (cond
   ((null? items) #t)
   (else 
     (proc (car items))
     (for-each proc (cdr items)))
  )
)