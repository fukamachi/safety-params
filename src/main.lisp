(uiop:define-package #:safety-params
  (:use #:cl
        #:safety-params/errors)
  (:shadowing-import-from #:safety-params/assertions
                          #:satisfies)
  (:use-reexport #:safety-params/conversions
                 #:safety-params/assertions
                 #:safety-params/errors))
(in-package #:safety-params)

