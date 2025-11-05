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


(defun check-fp-sort (title fn input expected) (format t "~:[FAILED~;passed~] ~a~%" (equal (funcall fn input) expected) title))

(defun test-fp-shaker-sort ()
  (check-fp-sort "fp-shaker: empty"  (lambda (xs) (fp-shaker-sort xs)) '() '())
  (check-fp-sort "fp-shaker: sorted"  (lambda (xs) (fp-shaker-sort xs)) '(1 2 3 4) '(1 2 3 4))
  (check-fp-sort "fp-shaker: reversed"(lambda (xs) (fp-shaker-sort xs)) '(5 4 3 2 1) '(1 2 3 4 5))
  (check-fp-sort "fp-shaker: dups&neg"(lambda (xs) (fp-shaker-sort xs)) '(3 -1 2 3 0 -1) '(-1 -1 0 2 3 3))
  (check-fp-sort "fp-shaker: key len, >" (lambda (xs) (fp-shaker-sort xs :key #'length :test #'>)) '("a" "bbb" "cc" "dddd") '("dddd" "bbb" "cc" "a")))




(defun propagator-fn (&key (comparator #'>))
  (let ((have-prev nil) (prev nil))
    (lambda (x)
      (if (not have-prev)
          (setf have-prev t prev x)
          (if (funcall comparator x prev)
              (setf prev x)
              prev)))))


(defun check-propagator (title mapper input expected) (format t "~:[FAILED~;passed~] ~a~%" (equal (mapcar mapper input) expected) title))

(defun test-propagator-fn ()
  (check-propagator "prop: sorted" (propagator-fn) '(1 2 3) '(1 2 3))
  (check-propagator "prop: odds higher" (propagator-fn) '(3 1 4 2) '(3 3 4 4))
  (check-propagator "prop: comparator < (min-propagation)" (propagator-fn :comparator #'<) '(5 4 7 3 6) '(5 4 4 3 3))
  (check-propagator "prop: negatives & zeros" (propagator-fn) '(-2 0 -1 3 -5 2) '(-2 0 0 3 3 3))
  (check-propagator "prop: strings, string>" (propagator-fn :comparator #'string>) '("a" "bb" "ab" "zzz" "m") '("a" "bb" "bb" "zzz" "zzz")))



(defun run-all-tests ()
  (test-fp-shaker-sort)
  (test-propagator-fn)
  (format t "~&All tests finished.~%")
  :ok)
