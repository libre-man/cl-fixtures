(in-package :cl-fixtures-test)

(defmacro collect-locked-parameters (&rest parameters)
  (with-gensyms (res)
    (let ((vars (loop :for _ :in (car parameters)
                      :collect (gensym "PARAM-"))))
      `(let ((,res nil))
         (with-locked-parameters ,vars ,(loop :for param :in parameters
                                              :collect `',param)
           (push (list ,@vars) ,res))
         (nreverse ,res)))))

(defmacro collect-parameters (&rest parameters)
  (with-gensyms (res)
    (let* ((lambda-list (loop :for param :in parameters
                              :collect (list (gensym "PARAM-") param)))
           (vars (mapcar #'car lambda-list)))
      `(let ((,res nil))
         (with-parameters ,lambda-list
           (push (list ,@vars) ,res))
         (setf ,res (nreverse ,res))
         ,res))))

(plan nil)

(subtest "Testing single parameter"
  (is (collect-parameters '(1 2 3))
      '((1) (2) (3)))
  (is (collect-parameters #(4 5 6))
      '((4) (5) (6)))
  (is (collect-parameters (loop :for i :from 10 :below 25
                                :collect i))
      (loop :for i :from 10 :below 25
            :collect (list i)))
  (with-parameters ((a nil))
    (declare (ignore a))
    (fail "This should not execute.")))

(subtest "Testing with no parameters"
  (let ((exec nil))
    (with-parameters ()
      (if exec
          (fail "This should only execute once.")
          (setf exec t)))
    (is exec t)))

(subtest "Testing multiple parameters"
  (let ((res (collect-parameters '(first 2 3 4 5)
                                 #(6 7 8)
                                 (loop :for i :from 10 :below 20
                                       :collect i)))
        (expected nil))
    (dolist (a '(first 2 3 4 5))
      (dolist (b '(6 7 8))
        (loop :for c :from 10 :below 20
              :do (setf expected (cons (list a b c) expected)))))
    (setf expected (nreverse expected))
    (is res expected)))

(subtest "Testing locked parameters"
  (let ((executed nil))
    (is (collect-locked-parameters (1 2 3) (4 5 6))
        '((1 2 3) (4 5 6)))
    (with-locked-parameters (a b) ((list 2 3))
      (is (list a b)
          (list 2 3)))
    (with-locked-parameters () ()
      (if executed
          (fail "This should execute once")
          (setf executed t)))
    (ok executed "Empty parameters body should execute once")
    (is (collect-locked-parameters (1 2 3) (4 5 (0 1)))
        '((1 2 3) (4 5 (0 1))))
    (with-locked-parameters (a) ()
      (declare (ignore a))
      (fail "This can never execute"))))

(finalize)
