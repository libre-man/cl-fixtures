(in-package :cl-fixtures)

(defmacro with-locked-parameters (names bindings &body body)
  "Execute the given \"BODY\" with the given parameters named by \"NAMES\".

The forms \"BODY\" will be executed with the variables in \"NAMES\" bound to the
values in \"BINDINGS\". This \"BINDINGS\" form should be of the form
\"(combination-1 combination-2 ...)\". Each combination will be evaluated and
should return a list of the length of the list specified in the \"NAMES\"
variable. The first variable in \"NAMES\" will be bound to the first value in
the combination list, and the second value to the second item and so on.

The return value of the \"BODY\" is not preserved.

CL-FIXTURES> (collecting
               (with-locked-parameters (a b)
                   ('(1 2)
                    (list 3 4))
                 (collect a b)))
 #((1 2) (3 4))

If no parameters are specified \"BODY\" will be executed once. If there are
parameters specified but no bindings \"BODY\" will not be executed.
CL-FIXTURES> (collecting (with-locked-parameters () () (collect :one)))
 #(:one)
CL-FIXTURES> (collecting
               (with-locked-parameters (a) ()
                 (declare (ignore a))
                 (collect :one)))
 #()

Please note that the bindings and \"BODY\" are evaluated by turn. So first the
first binding, the first time \"BODY\" the second biding, \"BODY\" for the
second time and so on.

CL-FIXTURES> (with-output-to-string (str)
                 (with-locked-parameters (a) ((list (format str \"Hello\"))
                                              (list (format str \"Bye\")))
                   (declare (ignore a))
                   (format str \" Thomas.~%\")))
 \"Hello Thomas.
Bye Thomas.
\"
"
  (with-gensyms (func)
    `(flet ((,func ,names
              ,@body))
       (declare (dynamic-extent #',func))
       ,@(if (or names bindings)
             (loop :for bind :in bindings
                   :collect `(destructuring-bind (,@names) ,bind
                               (funcall #',func ,@names)))
             `((funcall #',func)))
       (values))))

(defmacro with-parameters (bindings &body body)
  "Execute the given body \"BODY\" with the Cartesian product of the bindings.

The \"BINDINGS\" is of the form \"(NAME VALUE)\" where \"NAME\" is the name of
the variable that will be bound in the body. The \"VALUE\" will be evaluated and
should return a function or sequence.

If the value returns a function this function should take one argument which is
the current continuation. This function should be called with the value that
should be yielded. If the value returns a sequence this sequence will be mapped
over, so \"NAME\" will be every item of this sequence.

This macro is essentially a way to make anonymous fixtures.

CL-FIXTURES> (collecting
               (with-parameters ((a (list 1 2))
                                 (b #(4 5 6))
                                 (c (lambda (cont) (funcall cont :next)
                                                   (funcall cont :item))))
                 (collect a b c)))
#((1 4 :next) (1 4 :item) (1 5 :next) (1 5 :item) (1 6 :next) (1 6 :item)
  (2 4 :next) (2 4 :item) (2 5 :next) (2 5 :item) (2 6 :next) (2 6 :item))

If no parameters are specified \"BODY\" will run once. If parameters are
specified but one of the sequences has a length of zero or the function does not
call the continuation \"BODY\" will not be executed.

CL-FIXTURES> (collecting (with-parameters () (collect :one)))
 #(:one)
CL-FIXTURES> (collecting (with-parameters ((a nil)) (collect a)))
 #()
CL-FIXTURES> (collecting (with-parameters ((a nil) (b '(1 2))) (collect a b)))
 #()

The return value of \"BODY\" is ignored.
CL-FIXTURES> (with-parameters () :value)
 NIL
"
  (with-gensyms (parameter-table mapper-name func-name)
    (let ((names (loop :for (name _) :in bindings
                       :collect (list name name))))
      `(let ((,parameter-table (make-hash-table)))
         (declare (ignorable ,parameter-table))
         (flet ((,func-name ,(mapcar #'car names)
                  ,@body))
           (declare (dynamic-extent (function ,func-name)))

           ,@(loop :for (name binding) :in bindings
                   :for binding-name = (gensym "BINDING-NAME-")
                   :collect `(setf (gethash ',name ,parameter-table)
                                   (lambda (,mapper-name)
                                     (let ((,binding-name ,binding))
                                       (etypecase ,binding-name
                                         (function
                                          (funcall ,binding-name ,mapper-name))
                                         (sequence
                                          (map nil ,mapper-name ,binding-name)))))))
           (with-fixtures-rec nil ,names nil ,parameter-table #',func-name)
           (values))))))
