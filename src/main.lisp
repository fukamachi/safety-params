(uiop:define-package #:safety-params
  (:use #:cl
        #:safety-params/errors)
  (:shadowing-import-from #:safety-params/assertions
                          #:satisfies
                          #:ignore-and-continue)
  (:use-reexport #:safety-params/conversions
                 #:safety-params/assertions
                 #:safety-params/errors)
  (:export #:sanitize))
(in-package #:safety-params)

(defmacro sanitize (form params)
  `(handler-bind ((safety-params-error
                    (lambda (e)
                      (let ((restart (find-restart 'ignore-and-continue e)))
                        (when restart
                          (invoke-restart restart))))))
     (funcall ,form ,params)))
