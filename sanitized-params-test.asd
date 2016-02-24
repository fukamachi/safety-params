#|
  This file is a part of sanitized-params project.
  Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage sanitized-params-test-asd
  (:use :cl :asdf))
(in-package :sanitized-params-test-asd)

(defsystem sanitized-params-test
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on (:sanitized-params
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "sanitized-params"))))
  :description "Test system for sanitized-params"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
