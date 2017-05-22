(in-package :cl-fixtures-test)

(declaim (optimize (debug 3)))

(plan nil)

(defmacro collect-cached-fixture-output (&rest fixtures)
  `(collect-fixture-output-base t ,fixtures))

(defmacro collect-fixture-output (&rest fixtures)
  `(collect-fixture-output-base nil ,fixtures))

(defmacro collect-fixture-output-base (cached fixtures)
  (let ((result (gensym "RESULT-"))
        (fixtures (loop :for fixture :in fixtures
                        :collect (list (gensym "FIXTURE-") fixture))))
    `(let ((,result nil))
       (,(if cached 'with-cached-fixtures 'with-fixtures) ,fixtures
        (push (list ,@(mapcar #'car fixtures)) ,result))
       (setf ,result (nreverse ,result))
       ,result)))

(subtest "Testing normal fixtures"
  (define-fixture test-fixture-1 mapper ()
    (loop :for i :below 10
          :do (funcall mapper i)))
  (define-fixture test-fixture-2 mapper ()
    (map nil mapper (loop :for i :from 10 :below 20
                          :collect i)))
  (let ((car-result '(0 1 2 3 4 5 6 7 8 9))
        (cdr-result '(10 11 12 13 14 15 16 17 18 19))
        (wanted-result nil)
        (result (collect-fixture-output test-fixture-1 test-fixture-2)))
    (dolist (car car-result)
      (dolist (cdr cdr-result)
        (push (list car cdr) wanted-result)))
    (setf wanted-result (nreverse wanted-result))
    (is result wanted-result)))

(subtest "Testing empty fixtures"
  (let ((done nil))
    (with-fixtures ()
      (when done
        (fail "This should not be executed!"))
      (setf done t))
    (is done t "Empty \"WITH-FIXTURES\" body should be executed only once.")))

(subtest "Testing simple fixtures"
  (define-simple-fixture test-simple-fixture-1 () nil
    'first)
  (define-simple-fixture test-simple-fixture-2 () nil
    'second)
  (define-simple-fixture test-simple-fixture-3 () nil
    "Do something else"
    (+ 1 2)
    (let ((result 'third))
      result))
  (is (collect-fixture-output test-simple-fixture-1
                              test-simple-fixture-2
                              test-simple-fixture-3)
      '((first second third))))

(subtest "Testing sequence fixtures"
  (define-sequence-fixture test-sequence-fixture-1 () nil
    '(0 1 2 3 4))
  (define-sequence-fixture test-sequence-fixture-2 () nil
    #(5 6 7 8 9))
  (define-sequence-fixture test-sequence-fixture-3 () nil
    (loop :for i :from 10 :below 15
          :collect i))
  (let ((first-result '(0 1 2 3 4))
        (second-result '(5 6 7 8 9))
        (third-result '(10 11 12 13 14))
        (wanted-result nil)
        (result (collect-fixture-output test-sequence-fixture-1
                                        test-sequence-fixture-2
                                        test-sequence-fixture-3)))
    (dolist (first first-result)
      (dolist (second second-result)
        (dolist (third third-result)
          (push (list first second third) wanted-result))))
    (setf wanted-result (nreverse wanted-result))
    (is result wanted-result)))

(subtest "Testing cleanup in fixtures"
  (let ((first-cleaned nil)
        (second-cleaned nil)
        (third-cleaned nil)
        (third-output '(1 2 3)))
    (define-fixture test-cleanup-fixture-1 mapper ()
      (funcall mapper 'first)
      (setf first-cleaned t))
    (define-simple-fixture test-cleanup-fixture-2 ()
        (lambda (res)
          (setf second-cleaned res))
      'second-res)
    (define-sequence-fixture test-cleanup-fixture-3 ()
        (lambda (res)
          (is res third-output :test #'eq)
          (setf third-cleaned 'third))
      third-output)
    (collect-fixture-output test-cleanup-fixture-1
                            test-cleanup-fixture-2
                            test-cleanup-fixture-3)
    (is first-cleaned t)
    (is second-cleaned 'second-res))

  (let ((fourth-cleaned nil)
        (fourth-ran nil))
    (define-fixture error-fixture shut-up ()
      (let ((_ shut-up))
        (declare (ignore _))
        (error "Help an error")))
    (define-simple-fixture test-cleanup-fixture-4 ()
        (lambda (fourth)
          (setf fourth-cleaned fourth))
      (setf fourth-ran 'fourth))
    (is-error
     (collect-fixture-output test-cleanup-fixture-4 error-fixture)
     'simple-error)
    (is fourth-cleaned 'fourth)
    (is fourth-ran 'fourth)
    (setf fourth-cleaned nil
          fourth-ran nil)
    (is-error
     (collect-fixture-output error-fixture test-cleanup-fixture-4)
     'simple-error)
    (is fourth-cleaned nil)
    (is fourth-ran nil)))

(subtest "Testing renaming capturing"
  (define-sequence-fixture test-fixture () nil
    #(1 2 3))
  (let ((output nil)
        (correct nil))
    (with-fixtures (test-fixture (test-fixture-alter test-fixture))
      (push (list test-fixture test-fixture-alter) output))
    (dolist (first '(1 2 3))
      (dolist (second '(1 2 3))
        (push (list first second) correct)))
    (is output correct)))

(subtest "Testing inheritance"
  (define-simple-fixture random-test-fixture () nil
    (random 10.0))
  (define-simple-fixture random-wrapper-test ((rand random-test-fixture)) nil
    (list rand))
  (isnt (list (collect-fixture-output random-test-fixture))
        (collect-fixture-output random-wrapper-test))
  (isnt (collect-fixture-output random-wrapper-test)
        (collect-fixture-output random-wrapper-test))

  (let ((res1 '(1 2 3 4))
        (res2 '(5 6 7 8))
        (output nil)
        (wanted nil))
    (define-sequence-fixture test-fixture-list-1 () nil
      res1)
    (define-sequence-fixture test-fixture-list-2 () nil
      res2)
    (define-fixture wrapper-list-fixture
        mapper
        ((list-1 test-fixture-list-1) (list-2 test-fixture-list-2))
      (funcall mapper (list list-1 list-2)))
    (setf output (collect-fixture-output wrapper-list-fixture))
    (dolist (car res1)
      (dolist (cdr res2)
        (push (list (list car cdr)) wanted)))
    (setf wanted (nreverse wanted))
    (is output wanted)))

(subtest "Testing non existing fixture"
  (flet ((error-test ()
           (collect-fixture-output ok-fixture)
           (is-error (collect-fixture-output using-non-existing)
                     'undefined-fixture)
           (is-error (collect-fixture-output non-existing-standalone)
                     'undefined-fixture)
           (is-error (collect-fixture-output ok-fixture non-existing-standalone)
                     'undefined-fixture)
           (is-error (collect-fixture-output non-existing-standalone ok-fixture)
                     'undefined-fixture)))
    (undefine-fixture not-yet-existing)
    (define-fixture using-non-existing mapper ((aaa not-yet-existing))
      (declare (ignore aaa))
      (funcall mapper 'nope))
    (define-simple-fixture ok-fixture () nil
      5)
    (error-test)
    (define-fixture not-yet-existing mapper () (funcall mapper nil))
    (is (collect-fixture-output using-non-existing)
        '((nope)))
    (undefine-fixture not-yet-existing)
    (error-test)))

(subtest "Testing return behavior"
  (let ((cleaned1 nil)
        (cleaned2 nil))
    (define-simple-fixture test-simple-return () (lambda (_)
                                                   (declare (ignore _))
                                                   (setf cleaned1 t))
      (return-from test-simple-return 'first)
      (fail "This should not execute"))
    (define-sequence-fixture test-sequence-return () (lambda (_)
                                                       (declare (ignore _))
                                                       (setf cleaned2 t))
      (return-from test-sequence-return '(1 2))
      (fail "This should not execute"))
    (is (collect-fixture-output test-simple-return test-sequence-return)
        '((first 1)
          (first 2)))
    (is cleaned1 t)
    (is cleaned2 t)))

(subtest "Testing cached fixtures"
  (define-sequence-fixture random-fixture () nil
    (loop :repeat 10
          :collect (random 100.0)))
  (define-simple-fixture add-fixture (random-fixture) nil
    (1+ random-fixture))
  (with-fixtures (random-fixture add-fixture)
    (when (= (- add-fixture random-fixture) 1)
      (fail "This should not be equal!")))
  (ok "Fixtures do not get cached")
  (with-cached-fixtures (random-fixture add-fixture)
    (unless (= (round (- add-fixture random-fixture)) 1)
      (fail "This should not be equal!")))
  (with-cached-fixtures (random-fixture)
    (with-fixtures (add-fixture)
      (unless (= (round (- add-fixture random-fixture)) 1)
        (fail "This should not be equal!"))))
  (with-cached-fixtures (add-fixture random-fixture)
    (when (= (- add-fixture random-fixture) 1)
      (fail "This should not be equal!")))
  (is (length (collect-fixture-output add-fixture random-fixture))
      (length (collect-cached-fixture-output add-fixture random-fixture)))
  (is (sqrt (length (collect-fixture-output random-fixture random-fixture)))
      (length (collect-cached-fixture-output random-fixture random-fixture))
      :test #'equalp))

(finalize)
