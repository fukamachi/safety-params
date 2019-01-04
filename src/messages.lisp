(defpackage #:safety-params/messages
  (:use #:cl)
  (:import-from #:safety-params/assertions
                #:validation-message))
(in-package #:safety-params/messages)

;; TODO: Add more messages for Common Lisp predicate functions and bundled ones.

(setf (validation-message 'listp) "Must be a list")
(setf (validation-message 'integerp) "Must be an integer")
