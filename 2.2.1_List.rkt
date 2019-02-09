#lang racket
;Упражнение 2.17.
;Определите процедуру last-pair, которая возвращает список, содержащий только последний
;элемент данного (непустого) списка.
(define (last-pair l)
  (cond
    ((empty? l) '())
    (( = (length l) 1) (car l))
    (else (last-pair (cdr l)))
  )
)

(last-pair (list 23 72 149 34))

;Упражнение 2.18.
;Определите процедуру reverse, которая принимает список как аргумент и возвращает список,
;состоящий из тех же элементов в обратном порядке:
(define (reverse l)
  (define (inner-r source target)
    (if (empty? source) target
     (inner-r (cdr source) (append (list (car source)) target))
    )
  )
 (inner-r l '())
)

(reverse (list 1 4 9 16 25))
;Упражнение 2.19.
;Рассмотрим программу подсчета способов размена из раздела 1.2.2. Было бы приятно иметь воз-
;можность легко изменять валюту, которую эта программа использует, так, чтобы можно было,
;например, вычислить, сколькими способами можно разменять британский фунт. Эта программа
;написана так, что знание о валюте распределено между процедурами first-denomination и
;count-change (которая знает, что существует пять видов американских монет). Приятнее было
;бы иметь возможность просто задавать список монет, которые можно использовать при размене.
;Мы хотим переписать процедуру cc так, чтобы ее вторым аргументом был список монет, а не
;целое число, которое указывает, какие монеты использовать. Тогда у нас могли бы быть списки,
;определяющие типы валют:
(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
;Можно было бы вызывать cc следующим образом:
;(cc 100 us-coins)
;292
;Это потребует некоторых изменений в программе cc. Ее форма останется прежней, но со вторым
;аргументом она будет работать иначе, вот так:
(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values))))
  )
;Определите процедуры first-denomination, except-first-denomination и no-more? в
;терминах элементарных операций над списковыми структурами. Влияет ли порядок списка coin-
;values на результат, получаемый cc? Почему?
(define (first-denomination v)
  (car v)
)
(define (except-first-denomination v)
  (cdr v)
)
(define (no-more? v)
  (empty? v)
)
(cc 100 us-coins)
;Упражнение 2.20.
;напишите процедуру same-parity, которая принимает одно или
;более целое число и возвращает список всех тех аргументов, у которых четность та же, что у
;первого аргумента. Например,
;(same-parity 1 2 3 4 5 6 7)
;(1 3 5 7)
;(same-parity 2 3 4 5 6 7)
;(2 4 6)

(define (same-parity first . other)
 (define (inner source target check)
   (cond
     ((empty? source) target)
     ((check (car source)) (inner (cdr source) (append target (list (car source))) check))
     (else (inner (cdr source) target check) )
   )
  )
  (if (odd? first) (inner other (list first) odd?)
    (inner other (list first) even?)
  )

)