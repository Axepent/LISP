(defun fi (i)
  (labels ((f (k)
             (cond ((= k 1) 1d0)
                   ((= k 11) 1d0)
                   ((and (>= k 2) (<= k 10)) (* (f (1- k)) (* (log k) 8d0)))
                   ((and (>= k 12) (<= k 20)) (* (f (1- k)) (/ (log k) 8d0)))
                   (t (error "i out of range: ~A" k)))))
    (f i)))

(defun table-iter ()
  (let ((res '()) (p 1d0))
    (loop for i from 2 to 10 do
          (setf p (* p (* (log i) 8d0)))
          (push (cons i p) res))
    (setf p 1d0)
    (loop for i from 12 to 20 do
          (setf p (* p (/ (log i) 8d0)))
          (push (cons i p) res))
    (nreverse res)))

(defun print-fi-table ()
  (let ((pairs
          (append
           (loop for i from 2 to 10 collect (cons i (fi i)))
           (loop for i from 12 to 20 collect (cons i (fi i))))))
    (format t "~&  i |            Fi~%")
    (format t "----+-----------------------~%")
    (dolist (p pairs)
      (format t "~3D | ~,12,5E~%" (car p) (cdr p)))
    (values)))


(defun strictly-increasing-p (xs)
  (every #'< xs (rest xs)))

(defun strictly-decreasing-p (xs)
  (every #'> xs (rest xs)))

(defun run-checks ()
  (let* ((tbl (table-iter))
         (lookup (lambda (k) (cdr (assoc k tbl))))
         (up  (mapcar (lambda (k) (funcall lookup k)) (loop for k from 2 to 10 collect k)))
         (dn  (mapcar (lambda (k) (funcall lookup k)) (loop for k from 12 to 20 collect k))))
    (format t "~&[TEST] F1=1: ~A~%"  (= (fi 1) 1d0))
    (format t "[TEST] F11=1: ~A~%" (= (fi 11) 1d0))
    (format t "[TEST] 2..10 strictly increasing: ~A~%" (strictly-increasing-p up))
    (format t "[TEST] 12..20 strictly decreasing: ~A~%" (strictly-decreasing-p dn))
    (values)))

