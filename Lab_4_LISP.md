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
(defun fp-selection-sort (seq &key (key #'identity) (test #'<))
  (let ((decorated (mapcar (lambda (x) (cons (funcall key x) x)) seq)))
    (labels ((pick-best (pairs)
               (reduce (lambda (best x)
                         (if (funcall test (car x) (car best)) x best))
                       pairs))
             (remove-once (x xs)
               (remove x xs :count 1 :test #'eq))
             (sel (pairs acc)
               (if (endp pairs)
                   (nreverse acc)
                   (let* ((best (pick-best pairs))
                          (rest (remove-once best pairs)))
                     (sel rest (cons (cdr best) acc))))))
      (sel decorated '()))))
```
### Тестові набори та утиліти першої частини
```lisp
(defun check-fp-sort (title fn input expected) (format t "~:[FAILED~;passed~] ~a~%" (equal (funcall fn input) expected) title))

(defun test-fp-selection-sort ()
  (check-fp-sort "fp: empty" (lambda (xs) (fp-selection-sort xs)) '() '())
  (check-fp-sort "fp: sorted" (lambda (xs) (fp-selection-sort xs)) '(1 2 3 4) '(1 2 3 4))
  (check-fp-sort "fp: reversed" (lambda (xs) (fp-selection-sort xs)) '(5 4 3 2 1) '(1 2 3 4 5))
  (check-fp-sort "fp: dups & negatives" (lambda (xs) (fp-selection-sort xs)) '(3 -1 2 3 0 -1) '(-1 -1 0 2 3 3))
  (check-fp-sort "fp: :key length, :test >" (lambda (xs) (fp-selection-sort xs :key #'length :test #'>)) '("a" "bbb" "cc" "dddd") '("dddd" "bbb" "cc" "a")))

```
### Тестування першої частини
```lisp
CL-USER> (test-fp-selection-sort)
passed fp: empty
passed fp: sorted
passed fp: reversed
passed fp: dups & negatives
passed fp: :key length, :test >
NIL
```

## Варіант другої частини №5


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