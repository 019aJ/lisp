#lang racket
(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor))))
)

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree-map sub-tree factor)
             (* sub-tree factor)))
       tree)
)

;Упражнение 2.30.
;Определите процедуру square-tree, подобную процедуре square-list из упражнения 2.21 как прямо (то есть без использования процедур высших порядков), так
;и с помощью map и рекурсии.
(define (square x) (* x  x))
(define (square-tree tree)
    (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree)))))
)
(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree-map sub-tree)
             (square sub-tree)))
       tree)
)
(square-tree
(list 1
(list 2 (list 3 4) 5)
(list 6 7)))
;(1 (4 (9 16) 25) (36 49))

;Упражнение 2.31.
;Абстрагируйте свой ответ на упражнение 2.30, получая процедуру tree-map, так, чтобы square-
;tree можно было определить следующим образом:
;(define (square-tree tree) (tree-map square tree))
(define (tree-map tree proc)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree proc)
             (proc sub-tree)))
       tree)
)
;Упражнение 2.32.
;Множество можно представить как список его различных элементов, а множество его подмножеств
;как список списков. Например, если множество равно (1 2 3), то множество его подмножеств
;равно (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Закончите следующее определение
;процедуры, которая порождает множество подмножеств и дайте ясное объяснение, почему она
;работает:
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (append x (list (car s)) )) rest))))
)