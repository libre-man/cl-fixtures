(in-package :cl-fixtures-test)

(declaim (optimize (debug 3)))

(plan 1)

(ok (incf-cl:doctest 'cl-fixtures) "All examples in docstrings should work")

(finalize)
