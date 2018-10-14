(defsystem "safety-params"
  :version "0.3.0"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("parse-number"
               "alexandria")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("conversions"
                                            "assertions"
                                            "errors"))
                 (:file "conversions" :depends-on ("errors"))
                 (:file "assertions" :depends-on ("errors"))
                 (:file "errors"))))
  :description "Filter parameters"
  :in-order-to ((test-op (test-op "safety-params/tests"))))

(defsystem "safety-params/tests"
  :author "Eitaro Fukamachi"
  :license "BSD 2-Clause"
  :depends-on ("safety-params"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for safety-params"
  :perform (test-op (op c) (symbol-call :rove '#:run c)))
