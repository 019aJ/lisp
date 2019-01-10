#lang racket
(require racket/include)
(require  "2.5.2_Coercion.rkt")

;Упражнение 2.81.
;Хьюго Дум заметил, что apply-generic может пытаться привести аргументы к типу друг друга
;даже тогда, когда их типы и так совпадают. Следовательно, решает он, нам нужно вставить
;в таблицу приведения процедуры, которые «приводят» аргументы каждого типа к нему самому.
;Например, в дополнение к приведению scheme-number->complex, описанному выше, он бы написал еще:
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)
;(put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number)
;(put-coercion 'complex 'complex complex->complex)
;а. Если установлены процедуры приведения типов, написанные Хьюго, что произойдет, когда
;apply-generic будет вызвана с двумя аргументами типа scheme-number или двумя аргументами типа complex для операции, которая не находится в таблице для этих типов? Допустим,
;процедуру возведения в степень. Что произойдет, если мы позовем exp с двумя комплексными числами в качестве аргументов?
(define c_a (make-complex-from-real-imag 1 3))
(define c_b (make-complex-from-real-imag 1 5))
(add c_a c_b)
;Произошло зацикливание
;(exp c_a c_b)

;б. Прав ли Хьюго, что нужно что-то сделать с приведением однотипных аргументов, или все и
;так работает правильно?
;и так работает
;в. Измените apply-generic так, чтобы она не пыталась применить приведение, если у обоих
;аргументов один и тот же тип.

;Упражнение 2.82.
;Покажите, как обобщить apply-generic так, чтобы она обрабатывала приведение в общем
;случае с несколькими аргументами. Один из способов состоит в том, чтобы попытаться сначала
;привести все аргументы к типу первого, потом к типу второго, и так далее. Приведите пример,
;когда эта стратегия (а также двухаргументная версия, описанная выше) недостаточно обща. (Подсказка: рассмотрите случай, когда в таблице есть какие-то подходящие операции со смешанными
;типами, но обращения к ним не произойдет.)

(define (apply-generic-many-args op . args)
  (define (cast-inner list-args list-type-tags new-args cast-type)
    (cond (
      (null? list-args) new-args)
      (else
       (let ((current-type (car list-type-tags)) (current-value (car list-args)) (coercion (get-coercion (car list-type-tags) cast-type)))
        (cond
          ((eq? current-type cast-type) (cast-inner (cdr list-args) (cdr list-type-tags) (append new-args (list current-value)) cast-type))
          (coercion (cast-inner (cdr list-args) (cdr list-type-tags) (append new-args  (list(coercion current-value))) cast-type))
          (else #f)
         )
        )
       )
     )
   )
  (define (find-first-cast type-tags args tail-types)
    (if (null? tail-types)
        ;все перебрали, не нашли рабочее приведение
        #f
        (let ((coerces-args (cast-inner args type-tags '() (car tail-types))))
          (if coerces-args
              ;приведение прошло успешно, возвращаем приведенные аргумены
              coerces-args
              ;не получилось - проверяем следующий тип
              (find-first-cast type-tags args (cdr tail-types))
           )
         )
     )
   )

 (let ((type-tags (map type-tag args)))
  (cond
    ((= (length args) 2)
     (let ((proc (get op type-tags)))
       (if proc (apply proc (map contents args))
           (let ((type1 (car type-tags)) (type2 (cadr type-tags)) (a1 (car args)) (a2 (cadr args)))
             (if (eq? type1 type2) (apply-generic op a1 a2)
                 (let ((t1->t2 (get-coercion type1 type2)) (t2->t1 (get-coercion type2 type1)))
                   (cond 
                     (t1->t2 (apply-generic-many-args op (t1->t2 a1) a2))
                     (t2->t1 (apply-generic-many-args op a1 (t2->t1 a2)))
                     (else (error "Нет метода для этих типов" (list op type-tags)))
                     )
                   )
                 )
             )        
           )
       )
     )
     ((= (length args) 1)
      (let ((proc (get op type-tags)))
       (if proc (apply proc (map contents args))  (error "Нет метода для этих типов" (list op type-tags)))
       ))
     (else
       ;приводим все к одному типу
        (let ((coerced-values (find-first-cast type-tags args type-tags)))
             (if coerced-values
                 (let ((type-tags (map type-tag coerced-values)))
                  (let ((proc (get op (list (car type-tags) (cadr type-tags)))))
                   ( apply-op-to-many op proc coerced-values)
                  ))
                 (error "Нет метода для этих типов"(list op type-tags))
              )
    )
      )
   )
 )
)

(apply-generic-many-args 'add  1 2 (make-complex-from-real-imag 1 3)(make-complex-from-real-imag 5 2) )

;Упражнение 2.83.
;Предположим, что Вы разрабатываете обобщенную арифметическую систему для работы с башней
;типов, показанной на рис. 2.25: целые, рациональные, действительные, комплексные. Для каждого
;из типов (кроме комплексного), разработайте процедуру, поднимающую объект на один уровень
;в башне. Покажите, как ввести обобщенную операцию raise, которая будет работать для всех
;типов (кроме комплексных чисел)
(raise 1)
(raise (make-rational 1 1))

;Упражнение 2.84.
;Используя операцию raise из упражнения 2.83, измените процедуру apply-generic так, чтобы она приводила аргументы к одному типу путем последовательного подъема, как описано в
;этом разделе. Потребуется придумать способ проверки, какой из двух типов выше по башне. Сделайте это способом, «совместимым» с остальной системой, так, чтобы не возникало проблем при
;добавлении к башне новых типов.

 (define (level type) 
   (cond ((eq? type 'scheme-number) 0) 
         ((eq? type 'rational) 1) 
         ((eq? type 'complex) 2) 
         (else (error "Invalid type: LEVEL" type)))) 
     
(define (apply-generic-level-up op . args)

  (define (find-max-level type-tags current-max)
    (if 
      (null? type-tags) current-max
      (let ((current-type-level (level (car type-tags))))
        (if (> current-type-level current-max) 
          (find-max-level (cdr type-tags) current-type-level)
          (find-max-level (cdr type-tags) current-max)
        )
      )
    )
  )
  
  (define (cast-value value cast-type)
    (let ((current-type-level (level (car (map type-tag (list value))))))
       (if (< current-type-level cast-type) 
         (cast-value (raise value) cast-type)
         value
       )
    )
  )  
  (define (cast-all list-args new-args cast-type)
    (if (null? list-args) 
      new-args
      (cast-all (cdr list-args) (append new-args (list (cast-value (car list-args) cast-type))) cast-type)
     )
  )



 (let ((type-tags (map type-tag args)))
  (cond
    ((= (length args) 1)
     (let ((proc (get op type-tags)))
       (if proc 
         (apply proc (map contents args))
         (error "Нет метода для этих типов" (list op type-tags)))
     )
    )
    (else
       ;ищем тип для приведения
      (let ((coerced-values (cast-all args '() (find-max-level type-tags 0))))
        (let ((type-tags (map type-tag coerced-values)))
           (let ((proc (get op (list (car type-tags) (cadr type-tags)))))
             (apply-op-to-many op proc coerced-values)
           )
	)
      )
    )
   )
 )
)
(define x1 3)
(define x2 (make-rational 1 1))
(define x3 (make-rational 3 2))
(define x4 (make-complex-from-real-imag 1 1))
(define x5 (make-complex-from-real-imag 5 0))
(define x6 (make-complex-from-real-imag (make-rational 1 1) 0))
(define x7 (make-complex-from-real-imag (make-rational 1 2) 0))
(apply-generic-level-up 'add x1 x2 x3 x4)
(apply-generic-many-args 'add x1 x2 x3 x4)

;Упражнение 2.85.
;В этом разделе упоминался метод «упрощения» объекта данных путем спуска его по башне насколько возможно вниз. Разработайте процедуру drop, которая делает это для башни, описанной
;в упражнении 2.83. Ключ к задаче состоит в том, что надо решить некоторым общим способом,
;можно ли понизить объект в типе. Например, комплексное число 1.5+0i можно опустить до real,
;комплексное число 1 + 0i до integer, а комплексное число 2 + 3i никуда понизить нельзя. Вот
;план того, как определить, можно ли понизить объект: для начала определите обобщенную операцию project, которая «сталкивает» объект вниз по башне. Например, проекция комплексного
;числа будет состоять в отбрасывании его мнимой части. Тогда число можно сдвинуть вниз в том
;случае, если, спроецировав его, а затем подняв обратно до исходного типа, мы получаем нечто,
;равное исходному числу. Покажите как реализовать эту идею в деталях, написав процедуру drop,
;которая опускает объект как можно ниже. Потребуется разработать различные операции проекции53 и установить project в системе в качестве обобщенной операции. Вам также потребуется
;обобщенный предикат равенства, подобный описанному в упражнении 2.79. Наконец, используя
;drop, перепишите apply-generic из упражнения 2.84, чтобы она «упрощала» свои результаты.

(define (drop x)
  (let ((projected (project x)))
    (if projected
		(drop projected)
		x
	)
  )
)
(display "Процедура drop")
(newline)
(display x1)
(display " -> ")
(display(drop x1))
(newline)
(display x2)
(display " -> ")
(display(drop x2))
(newline)
(display x3)
(display " -> ")
(display(drop x3))
(newline)
(display x4)
(display " -> ")
(display(drop x4))
(newline)
(display x5)
(display " -> ")
(display(drop x5))
(newline)
(display x6)
(display " -> ")
(display(drop x6))
(newline)
(display x7)
(display " -> ")
(display(drop x7))
(newline)

;Упражнение 2.86.
;Допустим, нам хочется работать с комплексными числами, чьи действительные и мнимые части,
;модули и аргументы могут быть обыкновенными числами, рациональными числами либо любыми
;другими, какие нам захочется добавить к системе. Опишите и реализуйте изменения в системе,
;которые потребуются, чтобы добавить такую возможность. Вам придется определить операции
;вроде sine (синус) и cosine (косинус), обобщенные на обыкновенные и рациональные числа.
(display "Упражнение 2.86")(newline)
(display "1^2 = ") (square x2)
(display "sqrt(1) = ") (sqrt-generic x2)
(display "atan(1) в радианах = ") (atan-one-arg-generic x2)
(display "atan(3/2) = ") (atan-generic x2 x3)
(display "cos(1) = ") (cos-generic x2)
(display "sin(1) = ") (sin-generic x2)

(display "1.5^2 = ") (square x3)
(display "sqrt(1.5) = ") (sqrt-generic x3)
(display "atan(1.5) в радианах = ") (atan-one-arg-generic x3)
(display "cos(1.5) = ") (cos-generic x3)
(display "sin(1.5) = ") (sin-generic x3)

