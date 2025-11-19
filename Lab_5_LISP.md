<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт з лабораторної роботи 5</b><br/>
"Робота з базою даних"<br/>
дисципліни "Вступ до функціонального програмування"
</p>
<p align="right"><b>Студент</b>: Гуманіцький Андрій Олександрович КВ-21</p>
<p align="right"><b>Рік</b>: 2025</p>

## Загальне завдання
В роботі необхідно реалізувати утиліти для роботи з базою даних, заданою за варіантом. База даних складається з кількох таблиць. Таблиці представлені у вигляді CSV файлів.
    1. Визначити структури та/або утиліти для створення записів з таблиць
    2. Розробити утиліту(-и) для зчитування таблиць з файлів
    3. Розробити функцію select , яка отримує на вхід шлях до файлу з таблицею, а також якийсь об'єкт, який дасть змогу зчитати записи конкретного типу або структури
    4. Написати утиліту(-и) для запису вибірки (списку записів) у файл
    5. Написати функції для конвертування записів у інший тип 
        - структури у геш-таблиці
        - геш-таблиці у асоціативні списки
        - асоціативні списки у геш-таблиці
    6. Написати функцію(-ї) для "красивого" виводу записів таблиці (pretty-print).

## Варіант 5
База даних: Проєкти із застосуванням ШІ  
Тип записів: Геш-таблиця

## Лістинг реалізації завдання
```lisp
    ;parsers for different types 
    (defun parse-csv-line (str parsers)
    (let* ((raw-cells (uiop:split-string str :separator '(#\;)))
            (cells (mapcar (lambda (s)
                            (string-trim '(#\Space #\Tab #\Return #\Newline) s))
                            raw-cells)))
        (mapcar #'funcall parsers cells)))

    (defun parse-float (str)
    (car (multiple-value-list (read-from-string str))))

    (defun parse-int-or-nil (str)
    (if (string= str "")
        nil
        (parse-integer str)))

    (defun parse-float-or-nil (str)
    (if (string= str "")
        nil
        (parse-float str))) 

    (defun parse-string-or-nil (str)
    (if (string= str "")
        nil
        str))


    ;structure for model and project csv-s
    (defun make-model-ht (id name type year)
    (let ((ht (make-hash-table :test 'eq)))
        (setf (gethash :id ht) id
            (gethash :name ht) name
            (gethash :type ht) type
            (gethash :year ht) year)
        ht))

    (defun make-project-ht (id name model-id budget-kusd customer status)
    (let ((ht (make-hash-table :test 'eq)))
        (setf (gethash :id ht) id
            (gethash :name ht) name
            (gethash :model-id ht) model-id
            (gethash :budget-kusd ht) budget-kusd
            (gethash :customer ht) customer
            (gethash :status ht) status)
        ht))




    ;csv-s to stings
    (defun csv-string->model-ht (str)
    (let* ((vals (parse-csv-line
                    str
                    (list #'parse-integer
                        #'identity
                        #'parse-string-or-nil
                        #'parse-int-or-nil)))
            (id   (nth 0 vals))
            (name (nth 1 vals))
            (type (nth 2 vals))
            (year (nth 3 vals)))
        (make-model-ht id name type year)))

    (defun csv-string->project-ht (str)
    (let* ((vals (parse-csv-line
                    str
                    (list #'parse-integer    
                        #'identity            
                        #'parse-integer       
                        #'parse-float-or-nil  
                        #'parse-string-or-nil 
                        #'identity)))         
            (id          (nth 0 vals))
            (name        (nth 1 vals))
            (model-id    (nth 2 vals))
            (budget-kusd (nth 3 vals))
            (customer    (nth 4 vals))
            (status      (nth 5 vals)))
        (make-project-ht id name model-id budget-kusd customer status)))




    ;read tables and path to them
    (defun read-table-from-file (path row-parser)
    (with-open-file (s path)
        (read-line s nil nil)
        (let ((rows '()))
        (loop for line = (read-line s nil nil)
                while line
                unless (string= line "")
                do (push (funcall row-parser line) rows)
                finally (return (nreverse rows))))))


    (defparameter *data-dir*
        #P"C:/Users/Andrew/portacle/projects/lab5/")

    (defun models-path ()
        (merge-pathnames "models.csv" *data-dir*))

    (defun projects-path ()
        (merge-pathnames "projects.csv" *data-dir*))



    ;check if everything is fine (as expected fields)
    (defun matching-model-p (ht &key id name type year)
    (and (if id
            (eql id (gethash :id ht))
            t)
        (if name
            (string= name (gethash :name ht))
            t)
        (if type
            (let ((tval (gethash :type ht)))
                (and tval (string= type tval)))
            t)
        (if year
            (eql year (gethash :year ht))
            t)))

    (defun matching-project-p (ht
                            &key id name model-id
                                    budget-min budget-max
                                    customer status)
    (and (if id
            (eql id (gethash :id ht))
            t)
        (if name
            (string= name (gethash :name ht))
            t)
        (if model-id
            (eql model-id (gethash :model-id ht))
            t)
        (if customer
            (let ((cval (gethash :customer ht)))
                (and cval (string= customer cval)))
            t)
        (if status
            (string= status (gethash :status ht))
            t)
        (if budget-min
            (let ((b (gethash :budget-kusd ht)))
                (and b (<= budget-min b)))
            t)
        (if budget-max
            (let ((b (gethash :budget-kusd ht)))
                (and b (<= b budget-max)))
            t)))

    ;select
    (defun select (path kind)
    (ecase kind
        (:models
        (lambda (&key id name type year)
        (let ((rows (read-table-from-file path #'csv-string->model-ht)))
            (remove-if-not
            (lambda (ht)
                (matching-model-p ht :id id :name name :type type :year year))
            rows))))
        (:projects
        (lambda (&key id name model-id budget-min budget-max customer status)
        (let ((rows (read-table-from-file path #'csv-string->project-ht)))
            (remove-if-not
            (lambda (ht)
                (matching-project-p ht
                                    :id id :name name :model-id model-id
                                    :budget-min budget-min :budget-max budget-max
                                    :customer customer :status status))
            rows))))))




    (defun write-selection-to-file (path records kind)
    (with-open-file (s path
                        :direction :output
                        :if-exists :supersede
                        :if-does-not-exist :create)
        (ecase kind
        (:models
        (format s "id;name;type;year~%")
        (dolist (ht records)
            (format s "~A;~A;~A;~A~%"
                    (or (gethash :id ht) "")
                    (or (gethash :name ht) "")
                    (or (gethash :type ht) "")
                    (or (gethash :year ht) ""))))
        (:projects
        (format s "id;name;model-id;budget-kusd;customer;status~%")
        (dolist (ht records)
            (format s "~A;~A;~A;~A;~A;~A~%"
                    (or (gethash :id ht) "")
                    (or (gethash :name ht) "")
                    (or (gethash :model-id ht) "")
                    (or (gethash :budget-kusd ht) "")
                    (or (gethash :customer ht) "")
                    (or (gethash :status ht) "")))))))




    (defun hash-table-to-alist (ht)
    (let (result)
        (maphash (lambda (key value)
                (push (cons key value) result))
                ht)
        result))

    (defun alist-to-hash-table (alist &key (test 'eq))
    (let ((ht (make-hash-table :test test)))
        (dolist (pair alist ht)
        (setf (gethash (car pair) ht) (cdr pair)))))






    (defun pretty-print-models (records &optional (stream *standard-output*))
    (format stream "~&~3A  ~20A  ~6A  ~4A~%"
            "ID" "NAME" "TYPE" "YEAR")
    (format stream "~A~%"
            "-------------------------------------------")
    (dolist (ht records)
        (format stream "~3A  ~20A  ~6A  ~4A~%"
                (or (gethash :id ht) "")
                (or (gethash :name ht) "")
                (or (gethash :type ht) "")
                (or (gethash :year ht) ""))))

    (defun pretty-print-projects (records &optional (stream *standard-output*))
    (format stream "~&~3A  ~40A  ~8A  ~12A  ~30A  ~10A~%"
            "ID" "NAME" "MODEL-ID" "BUDGET" "CUSTOMER" "STATUS")
    (format stream "~A~%"
            (make-string 115 :initial-element #\-))
    (dolist (ht records)
        (format stream "~3A  ~40A  ~8A  ~12A  ~30A  ~10A~%"
                (or (gethash :id ht) "")
                (or (gethash :name ht) "")
                (or (gethash :model-id ht) "")
                (let ((b (gethash :budget-kusd ht)))
                (if b
                    (format nil "~,2F" b)
                    ""))
                (or (gethash :customer ht) "")
                (or (gethash :status ht) ""))))


    (defun pretty-print (records kind &optional (stream *standard-output*))
    (ecase kind
        (:models   (pretty-print-models records stream))
        (:projects (pretty-print-projects records stream))))

```
### Тестові набори та утиліти
```lisp

    ;tests
    (defun check-test (title condition)
    (format t "~:[FAILED~;passed~] ~a~%" condition title)
    condition)

    (defun test-csv-string->model-ht ()
    (format t "~&== test-csv-string->model-ht ==~%")
    (let* ((line "1;GPT-4;LLM;2023")
            (ht (csv-string->model-ht line))
            (ok t))
        (setf ok (and ok (check-test "model/id"
                                    (eql (gethash :id ht) 1)))
            ok (and ok (check-test "model/name"
                                    (string= (gethash :name ht) "GPT-4")))
            ok (and ok (check-test "model/type"
                                    (string= (gethash :type ht) "LLM")))
            ok (and ok (check-test "model/year"
                                    (eql (gethash :year ht) 2023))))
        ok))

    (defun test-csv-string->project-ht ()
    (format t "~&== test-csv-string->project-ht ==~%")
    (let* ((line "1;Dynamic NPC Dialogues;1;350.0;Kepler Interactive;process")
            (ht (csv-string->project-ht line))
            (ok t))
        (setf ok (and ok (check-test "project/id"
                                    (eql (gethash :id ht) 1)))
            ok (and ok (check-test "project/name"
                                    (string= (gethash :name ht)
                                            "Dynamic NPC Dialogues")))
            ok (and ok (check-test "project/model-id"
                                    (eql (gethash :model-id ht) 1)))
            ok (and ok (check-test "project/budget-kusd"
                                    (= (gethash :budget-kusd ht) 350.0d0)))
            ok (and ok (check-test "project/customer"
                                    (string= (gethash :customer ht)
                                            "Kepler Interactive")))
            ok (and ok (check-test "project/status"
                                    (string= (gethash :status ht) "process"))))
        ok))


    (defun test-read-table-from-file-models ()
    (format t "~&== test-read-table-from-file-models ==~%")
    (let* ((models (read-table-from-file (models-path)
                                        #'csv-string->model-ht))
            (ok t))
        (setf ok (and ok (check-test "models/listp"
                                    (listp models)))
            ok (and ok (check-test "models/not-empty"
                                    (> (length models) 0)))
            ok (and ok (check-test "models/first-hash-table"
                                    (hash-table-p (first models)))))
        ok))

    (defun test-read-table-from-file-projects ()
    (format t "~&== test-read-table-from-file-projects ==~%")
    (let* ((projects (read-table-from-file (projects-path)
                                            #'csv-string->project-ht))
            (ok t))
        (setf ok (and ok (check-test "projects/listp"
                                    (listp projects)))
            ok (and ok (check-test "projects/not-empty"
                                    (> (length projects) 0)))
            ok (and ok (check-test "projects/first-hash-table"
                                    (hash-table-p (first projects)))))
        ok))


    (defun test-select-models ()
    (format t "~&== test-select-models ==~%")
    (let* ((sel (select (models-path) :models))
            (all (funcall sel))
            (ok t))
        (setf ok (and ok (check-test "select/models/listp"
                                    (listp all)))
            ok (and ok (check-test "select/models/not-empty"
                                    (> (length all) 0))))
        (when all
        (let* ((first (first all))
                (id (gethash :id first))
                (by-id (funcall sel :id id)))
            (setf ok (and ok (check-test "select/models/by-id/listp"
                                        (listp by-id)))
                ok (and ok (check-test "select/models/by-id/not-empty"
                                        (> (length by-id) 0)))
                ok (and ok (check-test "select/models/by-id/all-same-id"
                                        (every (lambda (ht)
                                                (eql (gethash :id ht) id))
                                                by-id))))))
        ok))

    (defun test-select-projects ()
    (format t "~&== test-select-projects ==~%")
    (let* ((sel (select (projects-path) :projects))
            (all (funcall sel))
            (ok t))
        (setf ok (and ok (check-test "select/projects/listp"
                                    (listp all)))
            ok (and ok (check-test "select/projects/not-empty"
                                    (> (length all) 0))))
        (when all
        (let* ((first (first all))
                (id (gethash :id first))
                (by-id (funcall sel :id id)))
            (setf ok (and ok (check-test "select/projects/by-id/listp"
                                        (listp by-id)))
                ok (and ok (check-test "select/projects/by-id/not-empty"
                                        (> (length by-id) 0)))
                ok (and ok (check-test "select/projects/by-id/all-same-id"
                                        (every (lambda (ht)
                                                (eql (gethash :id ht) id))
                                                by-id))))))
        ok))


    (defun test-pretty-print-models ()
    (format t "~&== test-pretty-print-models ==~%")
    (let* ((records (list
                    (make-model-ht 1 "GPT-4" "LLM" 2023)
                    (make-model-ht 2 "ResNet-50" "CNN" 2015)))
            (output (with-output-to-string (s)
                    (pretty-print-models records s)))
            (ok t))
        (setf ok (and ok (check-test "pp/models/not-empty"
                                    (> (length output) 0)))
            ok (and ok (check-test "pp/models/contains-GPT-4"
                                    (search "GPT-4" output)))
            ok (and ok (check-test "pp/models/contains-ResNet-50"
                                    (search "ResNet-50" output))))
        ok))


    (defun test-pretty-print-projects ()
    (format t "~&== test-pretty-print-projects ==~%")
    (let* ((records (list
                    (make-project-ht 1 "Dynamic NPC Dialogues"
                                        1 350.0d0 "Kepler Interactive" "process")
                    (make-project-ht 2 "Toxic Chat Filter"
                                        3 nil "Electronic Arts" "done")))
            (output (with-output-to-string (s)
                    (pretty-print-projects records s)))
            (ok t))
        (setf ok (and ok (check-test "pp/projects/not-empty"
                                    (> (length output) 0)))
            ok (and ok (check-test "pp/projects/contains-first"
                                    (search "Dynamic NPC Dialogues" output)))
            ok (and ok (check-test "pp/projects/contains-second"
                                    (search "Toxic Chat Filter" output))))
        ok))


    (defun run-all-tests ()
    (format t "~&Running tests...~%")
    (let ((ok t))
        (setf ok (and ok (test-csv-string->model-ht))
            ok (and ok (test-csv-string->project-ht))
            ok (and ok (test-read-table-from-file-models))
            ok (and ok (test-read-table-from-file-projects))
            ok (and ok (test-select-models))
            ok (and ok (test-select-projects))
            ok (and ok (test-pretty-print-models))
            ok (and ok (test-pretty-print-projects)))
        (format t "~&Summary: ~A~%" (if ok "ALL TESTS PASSED" "SOME TESTS FAILED"))
        ok))

```
### Тестування
```lisp
CL-USER> (run-all-tests)
Running tests...
== test-csv-string->model-ht ==
passed model/id
passed model/name
passed model/type
passed model/year
== test-csv-string->project-ht ==
passed project/id
passed project/name
passed project/model-id
passed project/budget-kusd
passed project/customer
passed project/status
== test-read-table-from-file-models ==
passed models/listp
passed models/not-empty
passed models/first-hash-table
== test-read-table-from-file-projects ==
passed projects/listp
passed projects/not-empty
passed projects/first-hash-table
== test-select-models ==
passed select/models/listp
passed select/models/not-empty
passed select/models/by-id/listp
passed select/models/by-id/not-empty
passed select/models/by-id/all-same-id
== test-select-projects ==
passed select/projects/listp
passed select/projects/not-empty
passed select/projects/by-id/listp
passed select/projects/by-id/not-empty
passed select/projects/by-id/all-same-id
== test-pretty-print-models ==
passed pp/models/not-empty
passed pp/models/contains-GPT-4
passed pp/models/contains-ResNet-50
== test-pretty-print-projects ==
passed pp/projects/not-empty
passed pp/projects/contains-first
passed pp/projects/contains-second
Summary: ALL TESTS PASSED
349

;pretty-print
CL-USER> (defparameter *projects*
  (read-table-from-file (projects-path) #'csv-string->project-ht))

*PROJECTS*
CL-USER> (pretty-print-projects *projects*)

ID   NAME                                      MODEL-ID  BUDGET        CUSTOMER                        STATUS    
-------------------------------------------------------------------------------------------------------------------
1    Dynamic NPC Dialogues                     1         350.00        Kepler Interactive              process   
2    Procedural Quest Generator                2         420.00        Square Enix                     idea      
3    Toxic Chat Filter                         3         180.00        Electronic Arts                 done      
4    Automatic Highlight Generator             4         250.00        Ubisoft                         process   
NIL


CL-USER> (defparameter *models*
  (read-table-from-file (models-path) #'csv-string->model-ht))
*MODELS*
CL-USER> (pretty-print-models *models*)
ID   NAME                  TYPE    YEAR
-------------------------------------------
1    GPT-4                 LLM     2023
2    GPT-5                 LLM     2025
3    Grok-4                LLM     2024
4    DeepSeek-V3           LLM     2024
5    ResNet-50             CNN     2015
NIL

;selection
CL-USER> (defparameter *sel-models*
           (select (models-path) :models))
*SEL-MODELS*
CL-USER> (funcall *sel-models*)
(#<HASH-TABLE :TEST EQ :COUNT 4 {10030FAF33}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {10030FB443}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {10030FB953}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {10030FBE83}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {10030FC3B3}>)
CL-USER> (defparameter *llm-models*
           (funcall *sel-models* :type "LLM"))
*LLM-MODELS*
CL-USER> (length *llm-models*)
4


CL-USER> (defparameter *sel-projects*
           (select (projects-path) :projects))
*SEL-PROJECTS*
CL-USER> (defparameter *in-process*
           (funcall *sel-projects* :status "process"))
*IN-PROCESS*
CL-USER> (mapcar (lambda (ht) (list (gethash :id ht)
                                    (gethash :name ht)))
                 *in-process*)
((1 "Dynamic NPC Dialogues") (4 "Automatic Highlight Generator"))


;write to file
CL-USER> (defparameter *llm*
           (funcall *sel-models* :type "LLM"))
*LLM*
CL-USER> (write-selection-to-file
          (merge-pathnames "llm-models.csv" *data-dir*)
          *llm*
          :models)
NIL
CL-USER> (read-table-from-file
          (merge-pathnames "llm-models.csv" *data-dir*)
          #'csv-string->model-ht)
(#<HASH-TABLE :TEST EQ :COUNT 4 {100314D963}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {100314DE43}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {100314E323}>
 #<HASH-TABLE :TEST EQ :COUNT 4 {100314E823}>)

<p align="center">
  <img src="img/lab5.jpg" alt="Створений файл llm-models.csv" width="70%">
</p>
```

