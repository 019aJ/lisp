#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")
(require  "2.5.3_Symbol_Algebra.rkt")
;Упражнение 2.87.
;Установите =zero? для многочленов в обобщенный арифметический пакет. Это позволит adjointerm работать с многочленами, чьи коэффициенты сами по себе многочлены.
(define zero-terms 
  (list (make-term 4 0) 
        (make-term 2 0)
        (make-term 0 0)))
(define +ve-terms 
  (list (make-term 100 1)
        (make-term   2 2)
        (make-term   0 1)))
(define -ve-terms 
  (list (make-term 100 -1)
        (make-term   2 -2)
        (make-term   0 -1)))
(define term-101 (make-term 101 3))
 
(define pt0  (make-polynomial 'x zero-terms))
(define pt1  (make-polynomial 'x +ve-terms))
(define pt2  (make-polynomial 'y +ve-terms))
(define pt3  (make-polynomial 'x (cons term-101 +ve-terms)))
(define -pt1 (make-polynomial 'x -ve-terms))
(define -pt2 (make-polynomial 'y -ve-terms))
 
(define poly-py1 (make-polynomial 'y (list (make-term 3 pt1) (make-term 1 pt2) (make-term 0 pt3))))
(define poly-py2 (make-polynomial 'y (list (make-term 3 pt3) (make-term 2 pt1))))
(display "Is zero ")(display pt0)(display "? ")(display(zero? pt0))(newline)
(display "Is zero ")(display pt1)(display "? ")(display(zero? pt1))(newline)
(display "Is zero ")(display pt2)(display "? ")(display(zero? pt2))(newline)
(display "Is zero ")(display pt3)(display "? ")(display(zero? pt3))(newline)
(display "Is zero ")(display -pt1)(display "? ")(display(zero? -pt1))(newline)
(display "Is zero ")(display -pt2)(display "? ")(display(zero? -pt2))(newline)

(display "Is zero ")(display poly-py1)(display "? ")(display(zero? poly-py1))(newline)
(display "Is zero ")(display poly-py2)(display "? ")(display(zero? poly-py2))(newline)
(newline)

(display "sum of ")(display pt1)(display " and ")(display pt0)(display " is ")(display (add pt1 pt0))(newline)
(display "sum of ")(display pt1)(display " and ")(display pt3)(display " is ")(display (add pt1 pt3))(newline)
(display "sum of ")(display poly-py1)(display " and ")(display  poly-py2)(display " is ")(display (add poly-py1  poly-py2))(newline)
(display "mult of ")(display pt1)(display " and ")(display  pt3)(display " is ")(display (mul pt1  pt3))(newline)
(display "mult of ")(display pt0)(display " and ")(display  pt3)(display " is ")(display (mul pt0  pt3))(newline)
(newline)
;Упражнение 2.88.
;Расширьте систему многочленов так, чтобы она включала вычитание многочленов. (Подсказка:
;может оказаться полезным определить обобщенную операцию смены знака.)
(display "sub of ")(display pt1)(display " and ")(display pt1)(display " is ")(display (sub pt1 pt1))(newline)
(display "sub of ")(display pt1)(display " and ")(display pt0)(display " is ")(display (sub pt1 pt0))(newline)
(display "sub of ")(display pt1)(display " and ")(display -pt1)(display " is ")(display (sub pt1 -pt1))(newline)
(newline)
