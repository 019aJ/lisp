#lang racket
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x)))))
)

;Упражнение 2.27.
;Измените свою процедуру reverse из упражнения 2.18 так, чтобы получилась процедура deep-
;reverse, которая принимает список в качестве аргумента и возвращает в качестве значения
;список, где порядок элементов обратный и подсписки также обращены.

(define (deep-reverse l)
  (define (inner-r source target)
    (cond
      ((empty? source) target)
      ((pair? (car source)) (inner-r (cdr source) (append (list (inner-r (car source) '())) target)))
      (else (inner-r (cdr source) (append (list (car source)) target)))
    )
  )
 (inner-r l '())
)
(define x (list (list 1 2) (list 3 4)))

;Упражнение 2.28.
;Напишите процедуру fringe, которая берет в качестве аргумента дерево (представленное в ви-
;де списка) и возвращает список, элементы которого — все листья дерева, упорядоченные слева
;направо.

(define (fringe tree)
 (define (inner source target)
   (cond
     ((empty? source) target)
     ((pair? (car source)) (inner (cdr source) (append target  (inner (car source) '()))))
     (else (inner (cdr source) (append target (list (car source)))))
   )
 )
 (inner tree '())
)

;Упражнение 2.29.
;Бинарный мобиль состоит из двух ветвей, левой и правой. Каждая ветвь представляет собой
;стержень определенной длины, с которого свисает либо гирька, либо еще один бинарный мобиль.
;Мы можем представить бинарный мобиль в виде составных данных, соединив две ветви (например,
;с помощью list):
(define (make-mobile left right)(list left right))
;Ветвь составляется из длины length (которая должна быть числом) и структуры structure,
;которая может быть либо числом (представляющим простую гирьку), либо еще одним мобилем:
(define (make-branch length structure)(list length structure))
;а. Напишите соответствующие селекторы left-branch и right-branch, которые возвраща-
;ют левую и правую ветви мобиля, а также branch-length и branch-structure, которые
;возвращают компоненты ветви.
(define (left-branch m) (car m))
(define (right-branch m) (cadr m))
(define (branch-length b) (car b))
(define (branch-structure b) (cadr b))
;б. С помощью этих селекторов напишите процедуру total-weight, которая возвращает общий
;вес мобиля.
(define (mobile? m) (and (pair? m)(pair? (car m))))
(define (branch? m) (and (pair? m) (not (pair? (car m)))))
(define (total-weight mobile)
  (define (inner m weight)
    (cond
      ((empty? m) weight)
      ((mobile? m) (+ weight (inner (left-branch m) 0) (inner (right-branch m) 0)))
      ((branch? m) (+ weight (inner (branch-structure m) 0)))
      (else (+ weight m))
      )
  )
  (inner mobile 0)
)

(define m1 (make-mobile (make-branch 3 2) (make-branch 2 3)))
(define m2 (make-mobile (make-branch 1 6) (make-branch 2 3)))
(define m3 (make-mobile (make-branch 1 m1) (make-branch 3 m2)))
(define m4 (make-mobile (make-branch 2 m3) (make-branch 2 m1)))
(define m5 (make-mobile (make-branch 4 m3) (make-branch 1 m2)))
(define m6 (make-mobile (make-branch 1 m4) (make-branch 1 m5)))
(define m7 (make-mobile (make-branch 9 m1) (make-branch 5 m2)))
(define m8 (make-mobile (make-branch 1 7) (make-branch 1 7)))
(define m9 (make-mobile (make-branch 2 m7) (make-branch 2 m8)))
;в. Говорят, что мобиль сбалансирован, если момент вращения, действующий на его левую ветвь,
;равен моменту вращения, действующему на правую ветвь (то есть длина левого стержня, умножен-
;ная на вес груза, свисающего с него, равна соответствующему произведению для правой стороны),
;и если все подмобили, свисающие с его ветвей, также сбалансированы. Напишите предикат, кото-
;рый проверяет мобили на сбалансированность.
(define (balanced? mobile)
  (define (inner m weight)
    (cond
      ((not weight) #f)
      ((empty? m) weight)
      ((mobile? m)

       (let ((left-weight (inner (left-branch m) 0)) (left-l (branch-length (left-branch m))) (right-weight (inner (right-branch m) 0)) (right-l (branch-length (right-branch m))))
         (if (and left-weight right-weight (= (* left-l left-weight) (* right-l right-weight)))
             (+ weight left-weight right-weight)
             #f
         )
       ))
      ((branch? m)
       (let ((br-weight (inner (branch-structure m) 0)))
         (if br-weight
             (+ weight br-weight)
             #f
         )
       ))
      (else (+ weight m))
    )
  )
  (and (mobile? mobile) (number? (inner mobile 0)))
)

;г. Допустим, мы изменили представление мобилей, так что конструкторы теперь приняли такой
;вид:
(define (make-mobile-2 left right) (cons left right))
(define (make-branch-2 length structure)(cons length structure))
;Как много Вам нужно изменить в программах, чтобы перейти на новое представление?
(define (right-branch-2 m) (cdr m))
(define (branch-structure-2 b) (cdr b))