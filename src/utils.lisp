(defpackage #:safety-params/utils
  (:use #:cl)
  (:export #:function-name
           #:lambda-function-p))
(in-package #:safety-params/utils)

(defun function-name (function)
  #+sbcl (sb-impl::%fun-name function)
  #+ccl (ccl:function-name function)
  #+abcl (nth-value 2 (function-lambda-expression function))
  #-(or sbcl ccl abcl) nil)

(defun lambda-function-p (function)
  (let ((name (function-name function)))
    (and name
         (consp name)
         (eq (first name) 'cl:lambda))))
