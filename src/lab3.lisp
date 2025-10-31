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
         (cond
           ((or (null xs) (null (cdr xs))) (values xs nil))
           (t
            (multiple-value-bind (rest swapped) (backward-pass (cdr xs))
              (let ((a (car xs)) (b (car rest)))
                (if (<= a b)
                    (values (cons a rest) swapped)
                    (values (cons b (cons a (cdr rest))) t)))))))
       (shaker (xs)
         (multiple-value-bind (p1 s1) (forward-pass xs '() nil)
           (if (not s1) p1
               (multiple-value-bind (p2 s2) (backward-pass p1)
                 (if (not s2) p2 (shaker p2)))))))
    (shaker lst)))


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
         (prev-of (node)
           (let ((p head) (prev nil))
             (loop while (and p (not (eq p node))) do
                   (setf prev p
                         p    (cdr p)))
             prev))
         (pass-backward ()
           (cond
             ((or (null head) (null (cdr head))) nil)
             (t
              (let ((changed nil)
                    (right (last head)))
                (loop while (and right (not (eq right head))) do
                      (let ((prev (prev-of right)))
                        (when (> (car prev) (car right))
                          (rotatef (car prev) (car right))
                          (setf changed t))
                        (setf right prev)))
                changed)))))
      (loop while (or (pass-forward) (pass-backward)))
      head)))




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
