;Представление деревьев Хаффмана

;Листья дерева представляются в виде списка, состоящего из символа leaf (лист),
;символа, содержащегося в листе, и веса:
(define (make-leaf symbol weight)
  (list 'leaf symbol weight)
)
(define (leaf? object)
  (eq? (car object) 'leaf)
)
(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))


;порождение дерева
(define (make-code-tree left right)
  (list
    left
    right
    (append (symbols left) (symbols right))
    (+ (weight left) (weight right))
  )
)
;селекторы:
(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)
  )
)
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)
  )
)
;процедура реализует алгоритм декодирования. В качестве аргументов она
;принимает список из единиц и нулей, а также дерево Хаффмана.
(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
        (if (leaf? next-branch)
          (cons (symbol-leaf next-branch) (decode-1 (cdr bits) tree))
          (decode-1 (cdr bits) next-branch)
        )
      )
    )
  )
  (decode-1 bits tree)
)

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
    ((= bit 1) (right-branch branch))
    (else (error "плохой бит -- CHOOSE-BRANCH" bit))
  )
)

;представим множество листьев и деревьев как список элементов, упорядоченный
;по весу в возрастающем порядке
(define (adjoin-set x set)
  (cond ((null? set) (list x))
    ((< (weight x) (weight (car set))) (cons x set))
    (else (cons (car set) (adjoin-set x (cdr set))))
  )
)

;Следующая процедура принимает список пар вида символ–частота, например ((A 4) (B 2) (C 1) (D 1)),
; и порождает исходное упорядоченное множество листьев, готовое к слиянию по алгоритму Хаффмана:
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
      (adjoin-set (make-leaf (car pair) (cadr pair)) (make-leaf-set (cdr pairs)))
    )
  )
)
;-------------------------------------------
;Упражнение 2.67.
;Пусть нам даны дерево кодирования и пример сообщения:
(define sample-tree
(make-code-tree (make-leaf 'A 4)
(make-code-tree
(make-leaf 'B 2)
(make-code-tree (make-leaf 'D 1)
(make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0)
)
;Раскодируйте сообщение при помощи процедуры decode.
;Решение:
(newline)
(display "Упражнение 2.67.")
(newline)
(display "input:")
(newline)
(display "0110010101110")
(newline)
(display "decode:")
(newline)
(display (decode sample-message sample-tree))
(newline)
;-------------------------------------------

;-------------------------------------------
;Упражнение 2.68.
;Процедура encode получает в качестве аргументов сообщение и дерево, и порождает список
;битов, который представляет закодированное сообщение.
(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree) (encode (cdr message) tree))
  )
)
;Encode-symbol — процедура, которую Вы должны написать, возвращает список битов,
;который кодирует данный символ в соответствии с заданным деревом. Вы должны спроектировать
;encode-symbol так, чтобы она сообщала об ошибке, если символ вообще не содержится в дереве.
;Проверьте свою процедуру, закодировав тот результат, который Вы получили в упражнении 2.67,
;с деревом-примером и проверив, совпадает ли то, что получаете Вы, с исходным сообщением.
;Решение:
(define (encode-symbol symbol tree)
  (define (inner-encode symbol tree code)
    (if (leaf? tree) code
      (let ((l-tree (left-branch tree)) (r-tree (right-branch tree)))
        (cond
        ((list? (member symbol (symbols l-tree))) (inner-encode  symbol l-tree (append code (list 0))))
        ((list? (member symbol (symbols r-tree))) (inner-encode  symbol r-tree (append code (list 1))))
        (else (error "плохой символ -- CHOOSE-BRANCH" symbol))
        )
      )
    )
  )
(inner-encode symbol tree '())
)

(define (print-list to-print)
  (cond
    ((not (null? to-print))
      (display (car to-print))
      (print-list (cdr to-print))
    )
  )
)
(newline)
(display "Упражнение 2.68.")
(newline)
(display "encode:")
(newline)
(print-list (encode '(A D A B B C A) sample-tree))
(newline)
;-------------------------------------------

;-------------------------------------------
;Упражнение 2.69.
;Следующая процедура берет в качестве аргумента список пар вида символ-частота (где ни один символ не встречается более, чем в одной паре)
;и порождает дерево кодирования по Хаффману в соответствии с алгоритмом Хаффмана.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs))
)

;Приведенная выше процедура make-leaf-set преобразует список пар в упорядоченное множество пар.
;Вам нужно написать процедуру successive-merge, которая при помощи make-codetree сливает наиболее легкие элементы множества,
;пока не останется только один элемент, который и представляет собой требуемое дерево Хаффмана.


(define sample-pairs
'((A 4) (B 2) (C 1) (D 1))
)

(define (successive-merge pair-list)
  (define (inner-merge tail merged)
    (if (null? tail) merged
      (inner-merge (cdr tail) (make-code-tree (car tail) merged))
    )
  )
  (cond
    ((null? pair-list) '())
    ((null? (cdr pair-list)) (car pair-list))
    (else (inner-merge (cddr pair-list) (make-code-tree (car pair-list) (cadr pair-list))))
  )
)
;Решение:
(newline)
(display "Упражнение 2.69.")
(newline)
(display "Результат работы generate-huffman-tree:")
(newline)
(display (generate-huffman-tree sample-pairs))
(newline)
(display "Контрольное дерево:")
(newline)
(display sample-tree)
(newline)
;-------------------------------------------

;-------------------------------------------
;Упражнение 2.70.
;Нижеприведенный алфавит из восьми символов с соответствующими им относительными частотами был разработан, чтобы эффективно кодировать слова рок-песен 1950-х годов.
;A 2 NA 16;BOOM 1 SHA 3;GET 2 YIP 9;JOB 2 WAH 1
;При помощи generate-huffman-tree породите соответствующее дерево Хаффмана,
;и с помощью encode закодируйте следующее сообщение:
;Get a job
;Sha na na na na na na na na
;Get a job
;Sha na na na na na na na na
;Wah yip yip yip yip yip yip yip yip yip
;Sha boom
;Сколько битов потребовалось для кодирования? Каково наименьшее число битов, которое потребовалось бы для кодирования этой песни, если использовать код с фиксированной длиной для
;алфавита из восьми символов?
(define rock50
'((A 2) (NA 16) (BOOM 1) (SHA 3) (GET 2) (YIP 9) (JOB 2) (WAH 1))
)

(define rock50-ht
(generate-huffman-tree rock50)
)

(define song
'(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM)
)
;Решение:
(newline)
(display "Упражнение 2.70.")
(newline)
(display "Результат работы generate-huffman-tree:")
(newline)
(display "Сообщение:")
(newline)
(print-list (encode song rock50-ht))
(newline)
(display (length (encode song rock50-ht)))
(newline)
;-------------------------------------------

;-------------------------------------------
;Упражнение 2.71.
;Допустим, у нас есть дерево Хаффмана для алфавита из n символов, и относительные частоты
;символов равны 1, 2, 4, . . . , 2^(n−1). Изобразите дерево для n = 5; для n = 10. Сколько битов в таком
;дереве (для произвольного n) требуется, чтобы закодировать самый частый символ? Самый редкий
;символ?
(define task-tree-5
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8)))
)
(define task-tree-10
(generate-huffman-tree '((A 1) (B 2) (C 4) (D 8) (E 16) (F 32) (G 64) (E 128) (I 256)))
)
;Решение:
(newline)
(display "Упражнение 2.71.")
(newline)
(display task-tree-5)
(newline)
(display "Редкий: ")
(display (length (encode '(A) task-tree-5)))
(newline)
(display "Частый: ")
(display (length (encode '(D) task-tree-5)))
(newline)
(display task-tree-10)
(newline)
(display "Редкий: ")
(display (length (encode '(A) task-tree-10)))
(newline)
(display "Частый: ")
(display (length (encode '(I) task-tree-10)))
(newline)
;-------------------------------------------
