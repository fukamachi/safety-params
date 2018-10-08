(defsystem "sanitized-params"
  :version "0.2.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("closer-mop"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "sanitized-params" :depends-on ("error"))
                 (:file "error"))))
  :description "Sanitizer for parameters"
  :in-order-to ((test-op (test-op "sanitized-params/tests"))))

(defsystem "sanitized-params/tests"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("sanitized-params"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for sanitized-params"
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
