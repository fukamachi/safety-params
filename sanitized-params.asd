#|
  This file is a part of sanitized-params project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

#|
  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sanitized-params-asd
  (:use :cl :asdf))
(in-package :sanitized-params-asd)

(defsystem sanitized-params
  :version "0.1"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "sanitized-params"))))
  :description "Sanitizer for parameters"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sanitized-params-test))))
