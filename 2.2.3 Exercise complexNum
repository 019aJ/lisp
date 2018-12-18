#lang racket
(define *the-table* (make-hash));make THE table
(define (put key1 key2 value) (hash-set! *the-table* (list key1 key2) value));put
(define (get key1 key2) (hash-ref *the-table* (list key1 key2) #f));get
(define (attach-tag type-tag contents)(cons type-tag contents))


(define (square a)(* a a))

(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z) (car z))
(define (angle z) (cdr z))

(define (apply-generic op arg) (arg op))

(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond
      ((eq? op 'real-part) x)
      ((eq? op 'imag-part) y)
      ((eq? op 'magnitude)(sqrt (+ (square x) (square y))))
      ((eq? op 'angle) (atan y x))
      (else
      (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op))
    )
  )
  dispatch
)

;Упражнение 2.75.
;Реализуйте в стиле передачи сообщений конструктор make-from-mag-ang. Он должен быть
;аналогичен приведенной выше процедуре make-from-real-imag

(define (make-from-mag-ang x y)
  (define (dispatch op)
    (cond
     ((eq? op 'magnitude) x)
     ((eq? op 'angle) y)
     ((eq? op 'imag-part) (* x (sin y)))
     ((eq? op  'real-part) (* x (cos y)))
     (else (error "Неизвестная оп. -- MAKE-FROM-REAL-IMAG" op))
    )
  )
dispatch
)
