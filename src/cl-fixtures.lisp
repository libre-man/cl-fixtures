(in-package :cl-fixtures)

(define-condition undefined-fixture (undefined-function)
  ()
  (:report (lambda (condition stream)
             (with-accessors ((name cell-error-name)) condition
               (format stream "The fixture \"~A\" is undefined." name))))
  (:documentation "This condition will be signaled if a fixture is used that is
undefined. This will only happen if this fixture is used during runtime."))

(defvar *fixtures* (make-hash-table :test 'eq)
  "The hash-table where all the current fixtures are bound. They are stored with
their symbol (name) as their key and the function as the value. This variable
should only be modified by \"DEFINE*-FIXTURE\" macro's or when lexically
bound.")

(defmacro with-fixtures (fixtures &body body)
  "Execute the given \"BODY\" with the given \"FIXTURES\" bound in a implicit
progn.

The \"FIXTURES\" variable should be a list where each item can be of the
following forms:
- A symbol designating a fixture. This means bind the result of the fixture to
  the variable with the same name as the fixture.
- A list with two elements like \"(BIND NAME)\" where \"BIND\" will be bound to
  the result of the fixture designated by \"NAME\"

CL-FIXTURES> (progn (define-sequence-fixture simple () nil '(1 2))
                    (equalp (collecting (with-fixtures (simple)
                                          (collect simple)))
                            (collecting (with-fixtures ((a simple))
                                          (collect a)))))
 T

Multiple fixtures can occur multiple times in the fixture list, and this behaves
like one would suspect. However note that you have to alias at least one of them:
CL-FIXTURES> (progn (define-sequence-fixture simple () nil '(1 2))
                    (collecting (with-fixtures ((a simple) simple)
                                  (collect a simple))))
 #((1 1) (1 2) (2 1) (2 2))

This means that every fixture is evaluated every time it is being used. Take for
example this example:

CL-FIXTURES> (progn
               (define-fixture rand mapper () (funcall mapper (random 10.0)))
               (define-fixture rand2 mapper (rand) (funcall mapper rand))
               (collecting (with-fixtures (rand rand2) (collect (= rand rand2)))))
 #(NIL)

If this is not the behavior you want look at the \"WITH-CACHED-FIXTURES\" macro.

The return value of the \"BODY\" is ignored.
"
  `(prog1 (values)
     (with-fixtures-base ,fixtures nil ,@body)))

(defmacro with-cached-fixtures (fixtures &body body)
  "Execute the given \"BODY\" with the given \"FIXTURES\" with the values of the
fixtures cached.

The structure of \"FIXTURES\" is the same as in the \"WITH-FIXTURES\" macro.
However this macro does caching of the named fixtures in order. There are few
things to consider:

The first is that fixtures are evaluated, and therefore cached, in order. For
example:
CL-FIXTURES> (progn
               (define-fixture first mapper () (funcall mapper (random 10.0)))
               (define-fixture second mapper (first) (funcall mapper first))
               (list (collecting (with-cached-fixtures (first second) (collect (= first second))))
                     (collecting (with-cached-fixtures (second first) (collect (= first second))))
                     (collecting (with-fixtures (second first) (collect (= first second))))))
 (#(T) #(NIL) #(NIL))

In the first case the \"FIRST\" fixture is cached and replaced by the cached
version when we are executing the \"SECOND\" fixture. However as the fixture
list when defining the fixtures is a shorthand for \"WITH-FIXTURES\" it is not
cached when we reverse the order, so it has the same result as if
\"WITH-FIXTURES\" was used. So if only executing the fixture once is important
for more than just performance simply wrap all the code in
\"WITH-CACHED-FIXTURES\".

The second thing to consider is the using the same fixture multiple times will
NOT result a Cartesian product.
CL-FIXTURES> (progn
               (define-sequence-fixture test () nil '(1 2 3))
               (list (collecting (with-cached-fixtures ((a test) (b test))
                                   (collect a b)))
                     (collecting (with-cached-fixtures ((a test))
                                   (with-fixtures ((b test))
                                     (collect a b))))))
 (#((1 1) (2 2) (3 3)) #((1 1) (2 2) (3 3)))
"
  `(let ((*fixtures* (alexandria:copy-hash-table *fixtures*)))
     (with-fixtures-base ,fixtures t ,@body)
     (values)))

(defmacro with-fixtures-base (fixtures cachedp &body body)
  "The start point of creating a fixture.

This converts the given body to a
lambda and modifies the fixtures list to be in the form of \"((BIND-NAME
FIXTURE-NAME) ...)\". This list of fixtures is passed to \"WITH-FIXTURES-REC\".
"
  (with-gensyms (func-body)
    (let ((fixtures (mapcar
                     (lambda (el)
                       (cond
                         ((typep el 'symbol) (list el el))
                         ((and (listp el) (= (length el) 2)) el)
                         (t (error 'type-error
                                   :datum (write-to-string el)
                                   :expected-type "Symbol or proper list of length 2"))))
                     fixtures)))
      `(flet ((,func-body ,(mapcar #'car fixtures)
                ,@body))
         (declare (dynamic-extent (function ,func-body)))
         (symbol-macrolet ((,func-body #',func-body))
           (with-fixtures-rec ,cachedp ,fixtures nil *fixtures* ,func-body))))))

(defmacro with-fixtures-rec (cachedp todo done hash-table func)
  "Call the given \"FUNC\" using fixtures.

The \"TODO\" list consists of lists in the form of \"(BIND-NAME FIXTURE-NAME)\",
these are the fixtures that are not yet used. The \"DONE\" lists consists of a
list of \"BIND-NAME\" items which are the names of the variables to be used as
symbols. Using a single fixture is done by finding the fixture in the given
\"HASH-TABLE\" and calling this function with the expansion of
\"WITH-FIXTURES-REC\" as function. If no fixtures are left (the \"TODO\" list is
empty) the given function \"FUNC\" is called.

If \"CACHEDP\" is not nil the used fixtures will be cached.
"
  (if todo
      (with-gensyms (found-func foundp body-func current-value replace-fixture)
        (let* (((current-bind current-name) (pop todo))
               (done (cons current-bind done)))
          `(let* (((:mval ,found-func ,foundp) (gethash ',current-name ,hash-table))
                  ,@(when cachedp `((,current-value nil))))
             (unless ,foundp
               (error 'undefined-fixture :name ',current-name))
             (flet (,@(when cachedp
                        `((,replace-fixture (mapper)
                                            (funcall mapper ,current-value))))
                    (,body-func (,current-bind)
                      ,(when cachedp `(setf ,current-value ,current-bind))
                      (with-fixtures-rec ,cachedp ,todo ,done ,hash-table ,func)))
               ,@(when cachedp `((setf (gethash ',current-name ,hash-table)
                                       #',replace-fixture)))
               (unwind-protect
                    (funcall ,found-func
                             #',body-func)
                 ,(when cachedp
                    `(setf (gethash ',current-name ,hash-table)
                           ,found-func)))))))
      `(funcall ,func ,@(reverse done))))

(defmacro define-fixture (name mapper fixtures &body body)
  "Define a new normal fixture.

This is the basic function to define a new fixture named \"NAME\" with the given
body \"BODY\". This fixture can use the given fixtures \"FIXTURES\" which should
be a list in the same format as specified in the \"WITH-FIXTURES\" macro.

Within the given body a variable is bound by the name given in \"MAPPER\". This
is the continuation of the fixtures. So to yield a value one must call the
function in this variable (by doing a funcall or something). This function
accepts exactly one argument: the value that should be yielded.

The return value of \"BODY\" is ignored.

CL-FIXTURES> (define-fixture test-fixture cont () (funcall cont 5))
 TEST-FIXTURE
CL-FIXTURES> (list (define-fixture test-fixture cont () (funcall cont 5))
                   (define-fixture test-fixture cont () (funcall cont 6))
                   (collecting (with-fixtures (test-fixture) (collect test-fixture))))
 (TEST-FIXTURE TEST-FIXTURE #(6))
"
  (check-type name symbol)
  (check-type mapper symbol)
  (check-type fixtures list)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (gethash ',name *fixtures*)
           (lambda (,mapper)
             (with-fixtures ,fixtures
               ,@body)))
     ',name))

(defmacro define-sequence-fixture (name fixtures cleanup &body body)
  "Define a sequence fixture that returns a sequence.

Define a new sequence fixture with the name \"NAME\". This fixture will execute
\"BODY\" with the given fixtures \"FIXTURES\" (which should be a list in the
format described in \"WITH-FIXTURES\") and it should return a sequence. Each
element in this sequence is used as if it was yielded from the fixture. This
means that if a sequence fixture that returns #(1 2) is used the fixture will
have two values: 1 and 2.

After the fixtures is used the function given in \"CLEANUP\" is called, if
\"CLEANUP\" is \"NIL\" it will be ignored. This function should take exactly one
argument that is the result returned by the body. If there was a condition in
the body the cleanup function is not called. The cleanup is always called if the
body did return.

CL-FIXTURES> (list
               (define-sequence-fixture seq1 () nil (list 1 2 3))
               (collecting (with-fixtures (seq1) (collect seq1))))
 (SEQ1 #(1 2 3))
CL-FIXTURES> (let ((exec 0))
               (define-sequence-fixture seq2 ((item seq1))
                   (lambda (_) (declare (ignore _)) (incf exec))
                 (list item 4 5))
               (list (collecting (with-fixtures (seq2) (collect seq2)))
                     exec))
 (#(1 4 5 2 4 5 3 4 5) 3)
"
  (with-gensyms (mapper result)
    `(define-fixture ,name ,mapper ,fixtures
       (let ((,result (block ,name
                        ,@body)))
         (unwind-protect
              (map nil ,mapper ,result)
           (when ,cleanup
             (funcall ,cleanup ,result)))))))

(defmacro define-simple-fixture (name fixtures cleanup &body body)
  "Define a simple fixture that returns a single value.

Define a new simple fixture with the name \"NAME\". This fixture will execute
the \"BODY\" and \"CLEANUP\" as described in \"DEFINE-SEQUENCE-FIXTURE\".
However this time the body does not have to return a sequence but any value.
This value is used as the only value of the fixture.

CL-FIXTURES> (list
               (flet ((simple-func () :item))
                      (define-simple-fixture simpl1 () nil (simple-func)))
               (collecting (with-fixtures (simpl1) (collect simpl1))))
 (SIMPL1 #(:item))
"
  (with-gensyms (res)
    `(define-sequence-fixture ,name ,fixtures ,(if cleanup
                                                   `(lambda (,res)
                                                      (funcall ,cleanup
                                                               (car ,res)))
                                                   nil)
       (list (block ,name
               ,@body)))))

(defmacro undefine-fixture (name)
  "Undefine the given fixture \"NAME\".

If you try to undefine a fixture while it is in use the resulting behavior will
be unspecified.

CL-FIXTURES> (list
               (define-simple-fixture fixture () nil :item)
               (collecting (with-fixtures (fixture) (collect fixture)))
               (undefine-fixture fixture)
               (incf-cl:signals-p undefined-fixture
                 (collecting (with-fixtures (fixture) (collect fixture)))))
 (fixture #(:item) fixture T)
"
  `(progn
     (remhash ',name *fixtures*)
     ',name))
