(defun my-member (x lst)
  (and lst (or (eql (car lst) x)
               (my-member x (cdr lst)))))

(defun subsetp-by-eql (a b)
  (if (null a)
      t
      (and (my-member (car a) b)
           (subsetp-by-eql (cdr a) b))))

(defun set-equal-by-eql (a b)
  (and (subsetp-by-eql a b)
       (subsetp-by-eql b a)))

(defun remove-seconds (lst)
  (cond
    ((null lst) nil)
    ((null (cdr lst)) (list (car lst)))
    (t (cons (car lst) (remove-seconds (cddr lst))))))

(defun only-in (x y)
  (cond
    ((null x) nil)
    ((my-member (car x) y) (only-in (cdr x) y))
    (t (cons (car x) (only-in (cdr x) y)))))

(defun list-set-symmetric-difference (a b)
  (append (only-in a b) (only-in b a)))


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


(defun run-all-tests ()
  (test-rs)
  (test-lssd)
  (format t "~&All tests finished.~%")
  :ok)
