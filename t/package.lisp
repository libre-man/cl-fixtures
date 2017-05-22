(defpackage #:cl-fixtures-test
  (:use #:cl #:cl-fixtures #:prove)
  (:export #:collecting)
  (:import-from #:alexandria
                #:with-gensyms))
