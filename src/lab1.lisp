;; ЛР-1
;;1) making list general task
(defun make-my-list ()
  (cons 'A (list (list 'B 'C) () 7 "hi")))

;;9) making list 5 option (?)
(defun make-my-list-2 ()
  (let ((tail (list 'F )))
    (append (list 'D
                  (append (list 4 'E) tail)
                  (list 5 ))
            tail )))

(defun make-my-list-3 ()
  (let* ((tail (cons 'F nil))
         (sub2 (cons 4 (cons 'E tail)))
         (sub3 (cons 5 nil)))
    (cons 'D (cons sub2 (cons sub3 tail)))))


;;1) Save list 
(defvar T1)
(set 'T1 (make-my-list))

;;9) Save list
(defvar T2)
(set 'T2 (make-my-list-2))

(defvar T3)
(set 'T3 (make-my-list-3))

;;2) Head
(car T1)                    ; => A

;;3) Tail
(cdr T1)                    ; => ((B C) NIL 7 "hi")

;;4) 3rd element
(third T1)                  ; => NIL

;;5) last element
(car (last T1))             ; => "hi"

;;6) ATOM та LISTP
(atom (car T1))             ; => T   
(atom (cdr T1))             ; => NIL

(listp (cdr T1))            ; => T
(listp (car (last T1)))     ; => NIL
                                        
;;7) Different predicates
(numberp (first T1))        ; => NIL
(stringp (car (last T1)))   ; => T    
(symbolp (first T1))        ; => T 
(equal (second T1) '(B C))  ; => T


;;8) APPEND
(append T1 (second T1))     ; => (A (B C) NIL 7 "hi" B C)
