<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 2</b><br/>
"Рекурсія"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент(-ка)</b>: Гуманіцький Андрій Олександрович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
Реалізувати дві рекурсивні функції для обробки списків згідно варіанту.

## Варіант 5
Написати функцію remove-seconds , яка видаляє зі списку кожен другий елемент.

Написати функцію list-set-symmetric-difference , яка визначає симетричну різницю двох множин, заданих списками атомів (тобто, множину елементів, що не входять до обох множин):

## Лістинг функції remove-seconds
```lisp
(defun remove-seconds (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list (car lst)))
    (t (cons (car lst) (remove-seconds (cddr lst))))))
```
### Тестові набори та утиліти
```lisp
(defun check-rs (name input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (remove-seconds input) expected)
          name))

(defun test-rs ()
  (check-rs "rs test 1 (empty)"         '()                 '())
  (check-rs "rs test 2 (one)"           '(1)                '(1))
  (check-rs "rs test 3 (two)"           '(1 2)              '(1))
  (check-rs "rs test 4 (three)"         '(1 2 3)            '(1 3))
  (check-rs "rs test 5 (four)"          '(a b c d)          '(a c))
  (check-rs "rs test 6 (mixed)"         '(1 a 3 d 5 f)      '(1 3 5))
  (check-rs "rs test 7 (sublists)"      '((1) (2) (3) (4))  '((1) (3))))


```
## Лістинг функції list-set-symmetric-difference
```lisp
(defun my-member (x lst)
  (and lst (or (eql (car lst) x)
               (my-member x (cdr lst)))))

(defun list-set-symmetric-difference (a b)
  (labels
      ((only-in-a (x y)
         (if x
             (if (my-member (car x) y)
                 (only-in-a (cdr x) y)
                 (cons (car x) (only-in-a (cdr x) y)))
             nil))
       (only-in-b (y x)
         (if y
             (if (my-member (car y) x)
                 (only-in-b (cdr y) x)
                 (cons (car y) (only-in-b (cdr y) x)))
             nil)))
    (append (only-in-a a b) (only-in-b b a))))
```
### Тестові набори та утиліти
```lisp
(defun subsetp-by-eql (a b)
  (if (null a)
      t
      (and (my-member (car a) b)
           (subsetp-by-eql (cdr a) b))))

(defun set-equal-by-eql (a b)
  (and (subsetp-by-eql a b)
       (subsetp-by-eql b a)))

(defun check-lssd (name a b expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (set-equal-by-eql (list-set-symmetric-difference a b) expected)
          name))

(defun test-lssd ()
  (check-lssd "lssd test 1 (overlap 1)"  '(1 2 3 4) '(3 4 5 6) '(1 2 5 6))
  (check-lssd "lssd test 2 (disjoint)"   '(a b)     '(c d)     '(a b c d))
  (check-lssd "lssd test 3 (identical)"  '(1 2)     '(1 2)     '())
  (check-lssd "lssd test 4 (right)"      '()        '(x y)     '(x y))
  (check-lssd "lssd test 5 (left)"       '(x y)     '()        '(x y))
  (check-lssd "lssd test 6 (overlap 2)"  '(a b c)   '(b c d)   '(a d))
  (check-lssd "lssd test 7 (mixed)"      '(1 a 2)   '(a 3 4)   '(1 2 3 4)))
```
### Тестування
```lisp
(defun run-all-tests ()
  (test-rs)
  (test-lssd)
  (format t "~&All tests finished.~%")
  :ok)

CL-USER> (run-all-tests)
passed rs test 1 (empty)
passed rs test 2 (one)
passed rs test 3 (two)
passed rs test 4 (three)
passed rs test 5 (four)
passed rs test 6 (mixed)
passed rs test 7 (sublists)
passed lssd test 1 (overlap 1)
passed lssd test 2 (disjoint)
passed lssd test 3 (identical)
passed lssd test 4 (right)
passed lssd test 5 (left)
passed lssd test 6 (overlap 2)
passed lssd test 7 (mixed)
All tests finished.
:OK

```
