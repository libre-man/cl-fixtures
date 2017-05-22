#|
This file is a part of cl-fixtures project.
Copyright (c) 2017 Thomas Schaper (thomas@libremail.nl)
|#

(asdf:defsystem "cl-fixtures-test"
  :author "Thomas Schaper"
  :license "MIT"
  :depends-on ("cl-fixtures"
               "rutils"
               "incf-cl"
               "prove")
  :components ((:module "t"
                :components
                ((:test-file "parametrize" :depends-on ("package" "cl-fixtures" "util"))
                 (:test-file "cl-fixtures" :depends-on ("package" "util"))
                 (:test-file "doctests" :depends-on ("package" "util"))
                 (:test-file "util-test" :depends-on ("package" "util"))
                 (:file "util" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for cl-fixtures"

  :defsystem-depends-on ("prove-asdf")
  :perform (asdf:test-op (o c) (symbol-call :prove-asdf :run-test-system c)))
