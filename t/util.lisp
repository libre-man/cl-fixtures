(in-package :cl-fixtures)

(defmacro collecting (&body body)
  "Collect things into a vector.

Collecting is done by calling the function \"COLLECT\" within the given body
\"BODY\". The resulting vector will be returned.

CL-FIXTURES> (collecting (collect 1) (collect 2))
 #(1 2)
CL-FIXTURES> (collecting (format nil \"Not captured\"))
 #()
CL-FIXTURES> (with-output-to-string (str)
               (collecting (format str \"Outputed\")))
 \"Outputed\"
"
  (with-gensyms (res)
    `(let ((,res (make-array 0 :initial-contents nil
                               :adjustable t
                               :fill-pointer t)))
       (flet ((collect (&rest args)
                (if (cdr args)
                    (vector-push-extend args ,res)
                    (vector-push-extend (car args) ,res))))
         ,@body)
       ,res)))

(in-package :cl-fixtures-test)
(defmacro collecting (&body body) `(cl-fixtures::collecting ,body))
