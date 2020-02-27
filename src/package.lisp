(defpackage #:cl-fixtures
  (:use #:cl)
  (:import-from #:alexandria
                #:with-gensyms)
  (:export #:define-fixture
           #:undefine-fixture
           #:define-sequence-fixture
           #:define-simple-fixture
           #:undefined-fixture
           #:with-fixtures
           #:with-cached-fixtures
           #:with-parameters
           #:with-locked-parameters))
