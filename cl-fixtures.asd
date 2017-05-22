#|
This file is a part of cl-fixtures project.
Copyright (c) 2017 Thomas Schaper (thomas@libremail.nl)
|#

#|
A simple library to create and use parameterized fixtures

Author: Thomas Schaper (thomas@libremail.nl)
|#

(asdf:defsystem "cl-fixtures"
  :version "0.1"
  :author "Thomas Schaper"
  :license "MIT"
  :depends-on ("x.let-star" "alexandria")
  :components ((:module "src"
                :components
                ((:file "parametrize" :depends-on ("package" "cl-fixtures"))
                 (:file "cl-fixtures" :depends-on ("package"))
                 (:file "package"))))
  :description "A simple library to create and use parameterized fixtures"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.org"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-fixtures-test))))
