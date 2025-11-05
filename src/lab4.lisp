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


(defun check-fp-sort (title fn input expected) (format t "~:[FAILED~;passed~] ~a~%" (equal (funcall fn input) expected) title))

(defun test-fp-selection-sort ()
  (check-fp-sort "fp: empty" (lambda (xs) (fp-selection-sort xs)) '() '())
  (check-fp-sort "fp: sorted" (lambda (xs) (fp-selection-sort xs)) '(1 2 3 4) '(1 2 3 4))
  (check-fp-sort "fp: reversed" (lambda (xs) (fp-selection-sort xs)) '(5 4 3 2 1) '(1 2 3 4 5))
  (check-fp-sort "fp: dups & negatives" (lambda (xs) (fp-selection-sort xs)) '(3 -1 2 3 0 -1) '(-1 -1 0 2 3 3))
  (check-fp-sort "fp: :key length, :test >" (lambda (xs) (fp-selection-sort xs :key #'length :test #'>)) '("a" "bbb" "cc" "dddd") '("dddd" "bbb" "cc" "a")))




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
  (test-fp-selection-sort)
  (test-propagator-fn)
  (format t "~&All tests finished.~%")
  :ok)
