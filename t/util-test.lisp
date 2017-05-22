(in-package :cl-fixtures-test)

(plan 1)

(ok (incf-cl:doctest 'cl-fixtures-test:collecting) "The collecting helper macro should work")

(finalize)
