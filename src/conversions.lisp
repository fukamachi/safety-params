(defpackage #:safety-params/conversions
  (:use #:cl
        #:parse-number)
  (:import-from #:safety-params/errors
                #:conversion-failed)
  (:import-from #:alexandria
                #:ensure-car)
  (:export #:converts-into
           #:being*
           #:being))
(in-package #:safety-params/conversions)

(defgeneric converts-into (from to-type)
  (:method (from to-type)
    (cond
      ((typep from to-type) from)
      ((subtypep to-type 'string) (princ-to-string from))
      (t
       (error 'conversion-failed
              :value from
              :expected to-type)))))

(defmethod converts-into ((from string) (to-type (eql 'number)))
  (handler-case
      (parse-number from)
    (invalid-number ()
      (error 'conversion-failed
             :value from
             :expected to-type))))

(defmethod converts-into ((from string) to-type)
  (cond
    ((subtypep to-type 'integer)
     (handler-case (values (parse-integer from))
       (error () (error 'conversion-failed
                        :value from
                        :expected to-type))))
    ((subtypep to-type 'real)
     (handler-case (parse-real-number from)
       (error () (error 'conversion-failed
                        :value from
                        :expected to-type))))
    (t
     (call-next-method))))

(defstruct (conversion (:constructor make-conversion (name fn &optional default)))
  name
  fn
  default)

(defmethod print-object ((conversion conversion) stream)
  (print-unreadable-object (conversion stream :identity t)
    (format stream "CONVERSION to ~A"
            (conversion-name conversion))))

(defun being* (type default)
  (make-conversion
   type
   (lambda (param)
     (let ((value (converts-into param (ensure-car type))))
       (unless (typep value type)
         (error 'conversion-failed
                :value param
                :expected type))
       value))
   default))

(defmacro being (type &optional default)
  `(being* ',type ,default))
