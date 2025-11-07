<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гуманіцький Андрій Олександрович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
    Завдання складається з двох частин:
    1. Переписати функціональну реалізацію алгоритму сортування з лабораторної роботи 3 з такими змінами:
        - використати функції вищого порядку для роботи з послідовностями 
        - додати до інтерфейсу функції (та використання в реалізації) два ключових параметра: key та test 
    2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за варіантом

## Варіант першої частини №5
Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням

## Лістинг реалізації першої частини завдання
```lisp
(defun fp-shaker-sort (seq &key (key #'identity) (test #'<))
  (let ((xs (mapcar (lambda (x) (cons (funcall key x) x)) seq)))
    (labels
        ((swap-needed (a b) (funcall test (car b) (car a)))
         (forward-pass (lst acc swapped)
           (cond
             ((null lst) (values (nreverse acc) swapped))
             ((null (cdr lst)) (values (nreverse (cons (car lst) acc)) swapped))
             (t
              (let ((a (car lst)) (b (cadr lst)))
                (if (swap-needed a b)
                    (forward-pass (cons a (cddr lst)) (cons b acc) t)
                    (forward-pass (cons b (cddr lst)) (cons a acc) swapped))))))
         (backward-pass (lst)
           (cond
             ((or (null lst) (null (cdr lst))) (values lst nil))
             (t
              (multiple-value-bind (rest swapped) (backward-pass (cdr lst))
                (let ((a (car lst)) (b (car rest)))
                  (if (swap-needed a b)
                      (values (cons b (cons a (cdr rest))) t)
                      (values (cons a rest) swapped)))))))
         (shaker (lst)
           (multiple-value-bind (p1 s1) (forward-pass lst '() nil)
             (if (not s1)
                 p1
                 (multiple-value-bind (p2 s2) (backward-pass p1)
                   (if (not s2) p2 (shaker p2)))))))
      (mapcar #'cdr (shaker xs)))))
```
### Тестові набори та утиліти першої частини
```lisp
(defun check-fp-sort (title fn input expected) (format t "~:[FAILED~;passed~] ~a~%" (equal (funcall fn input) expected) title))

(defun test-fp-shaker-sort ()
  (check-fp-sort "fp-shaker: empty"  (lambda (xs) (fp-shaker-sort xs)) '() '())
  (check-fp-sort "fp-shaker: sorted"  (lambda (xs) (fp-shaker-sort xs)) '(1 2 3 4) '(1 2 3 4))
  (check-fp-sort "fp-shaker: reversed"(lambda (xs) (fp-shaker-sort xs)) '(5 4 3 2 1) '(1 2 3 4 5))
  (check-fp-sort "fp-shaker: dups&neg"(lambda (xs) (fp-shaker-sort xs)) '(3 -1 2 3 0 -1) '(-1 -1 0 2 3 3))
  (check-fp-sort "fp-shaker: key len, >" (lambda (xs) (fp-shaker-sort xs :key #'length :test #'>)) '("a" "bbb" "cc" "dddd") '("dddd" "bbb" "cc" "a")))

```
### Тестування першої частини
```lisp
CL-USER> (test-fp-shaker-sort)
passed fp-shaker: empty
passed fp-shaker: sorted
passed fp-shaker: reversed
passed fp-shaker: dups&neg
passed fp-shaker: key len, >
NIL
```

## Варіант другої частини №5
Написати функцію propagator-fn , яка має один ключовий параметр — функцію comparator . propagator-fn має повернути функцію, яка при застосуванні в якості першого аргументу mapcar разом з одним списком-аргументом робить наступне: якщо елемент не "кращий" за попередній згідно з comparator , тоді він заміняється на значення попереднього, тобто "кращого", елемента. Якщо ж він "кращий" за попередній елемент згідно comparator , тоді заміна не відбувається. Функція comparator за
замовчуванням має значення #'> .

## Лістинг реалізації другої частини завдання
```lisp
(defun propagator-fn (&key (comparator #'>))
  (let ((have-prev nil) (prev nil))
    (lambda (x)
      (if (not have-prev)
          (setf have-prev t prev x)
          (if (funcall comparator x prev)
              (setf prev x)
              prev)))))
```
### Тестові набори та утиліти другої частини
```lisp
(defun check-propagator (title mapper input expected) (format t "~:[FAILED~;passed~] ~a~%" (equal (mapcar mapper input) expected) title))

(defun test-propagator-fn ()
  (check-propagator "prop: sorted" (propagator-fn) '(1 2 3) '(1 2 3))
  (check-propagator "prop: odds higher" (propagator-fn) '(3 1 4 2) '(3 3 4 4))
  (check-propagator "prop: comparator < (min-propagation)" (propagator-fn :comparator #'<) '(5 4 7 3 6) '(5 4 4 3 3))
  (check-propagator "prop: negatives & zeros" (propagator-fn) '(-2 0 -1 3 -5 2) '(-2 0 0 3 3 3))
  (check-propagator "prop: strings, string>" (propagator-fn :comparator #'string>) '("a" "bb" "ab" "zzz" "m") '("a" "bb" "bb" "zzz" "zzz")))
```
### Тестування другої частини
```lisp
CL-USER> (test-propagator-fn)
passed prop: sorted
passed prop: odds higher
passed prop: comparator < (min-propagation)
passed prop: negatives & zeros
passed prop: strings, string>
NIL

```
