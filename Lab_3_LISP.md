<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 3</b><br/>
"Конструктивний і деструктивний підходи до роботи зі списками"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гуманіцький Андрій Олександрович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.

## Варіант 5
Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun shaker-sort-func (lst)
  (labels
      ((forward-pass (xs acc swapped)
         (cond
           ((null xs) (values (reverse acc) swapped))
           ((null (cdr xs))
            (values (reverse (cons (car xs) acc)) swapped))
           (t
            (let ((a (car xs)) (b (cadr xs)))
              (if (<= a b)
                  (forward-pass (cons b (cddr xs)) (cons a acc) swapped)
                  (forward-pass (cons a (cddr xs)) (cons b acc) t))))))
       (backward-pass (xs)
         (labels ((bp (ys)
                    (if (or (null ys) (null (cdr ys)))
                        (values ys nil)
                        (multiple-value-bind (rest swapped) (bp (cdr ys))
                          (let ((a (car ys)) (b (car rest)))
                            (if (<= a b)
                                (values (cons a rest) swapped)
                                (values (cons b (cons a (cdr rest))) t)))))))
           (bp xs)))
       (shaker (xs)
         (multiple-value-bind (p1 s1) (forward-pass xs '() nil)
           (if (not s1) p1
               (multiple-value-bind (p2 s2) (backward-pass p1)
                 (if (not s2) p2 (shaker p2)))))))
    (shaker lst)))
```

## Лістинг функції з використанням деструктивного підходу
```lisp
(defun shaker-sort-imp (lst)
  (let ((head (copy-list lst)))
    (labels
        ((pass-forward ()
           (let ((cur head) (changed nil))
             (loop while (and cur (cdr cur)) do
                   (let ((n (cdr cur)))
                     (when (> (car cur) (car n))
                       (rotatef (car cur) (car n))
                       (setf changed t)))
                   (setf cur (cdr cur)))
             changed))
         (pass-backward ()
           (let ((prev head)
                 (cur  (and head (cdr head)))
                 (changed nil))
             (loop while cur do
                   (when (< (car cur) (car prev))
                     (rotatef (car cur) (car prev))
                     (setf changed t))
                   (setf prev cur
                         cur  (cdr cur)))
             changed)))
      (loop
        with any = nil
        do (setf any (pass-forward))
           (when any
             (when (pass-backward)
               (setf any t)))
        unless any do (return head)))))
```

### Тестові набори, утиліти та тестування
```lisp
(defun check-sort (title fn input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (funcall fn input) expected)
          title))

(defun test-shaker ()
  (check-sort "func: empty"      #'shaker-sort-func '()                '())
  (check-sort "func: one"        #'shaker-sort-func '(1)               '(1))
  (check-sort "func: sorted"     #'shaker-sort-func '(1 2 3 4)         '(1 2 3 4))
  (check-sort "func: reversed"   #'shaker-sort-func '(4 3 2 1)         '(1 2 3 4))
  (check-sort "func: dups"       #'shaker-sort-func '(3 1 2 3 2 1)     '(1 1 2 2 3 3))
  (check-sort "func: negatives"  #'shaker-sort-func '(-2 5 0 -1 5)     '(-2 -1 0 5 5))
  (check-sort "imp: empty"       #'shaker-sort-imp  '()                '())
  (check-sort "imp: one"         #'shaker-sort-imp  '(1)               '(1))
  (check-sort "imp: sorted"      #'shaker-sort-imp  '(1 2 3 4)         '(1 2 3 4))
  (check-sort "imp: reversed"    #'shaker-sort-imp  '(4 3 2 1)         '(1 2 3 4))
  (check-sort "imp: dups"        #'shaker-sort-imp  '(3 1 2 3 2 1)     '(1 1 2 2 3 3))
  (check-sort "imp: negatives"   #'shaker-sort-imp  '(-2 5 0 -1 5)     '(-2 -1 0 5 5)))

CL-USER> (test-shaker)
passed func: empty
passed func: one
passed func: sorted
passed func: reversed
passed func: dups
passed func: negatives
passed imp: empty
passed imp: one
passed imp: sorted
passed imp: reversed
passed imp: dups
passed imp: negatives
NIL
```