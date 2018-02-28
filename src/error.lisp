(in-package :cl-user)
(defpackage sanitized-params.error
  (:use #:cl)
  (:export #:validation-error
           #:missing-keys
           #:invalid-keys
           #:unpermitted-keys
           #:missing-required-keys
           #:not-satisfied-key
           #:unpermitted-keys
           #:with-validation))
(in-package :sanitized-params.error)

(define-condition validation-error (error)
  ((missing :initarg :missing
            :initform '()
            :accessor missing-keys)
   (invalid :initarg :invalid
            :initform '()
            :accessor invalid-keys)
   (unpermitted :initarg :unpermitted
                :initform '()
                :accessor unpermitted-keys)))

(define-condition missing-required-keys ()
  ((keys :initarg :keys))
  (:report (lambda (condition stream)
             (format stream "Required keys are missing: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition not-satisfied-key ()
  ((key :initarg :key)
   (pred :initarg :pred))
  (:report (lambda (condition stream)
             (with-slots (key pred) condition
               (format stream "Key '~A' doesn't satisfy ~A"
                       key pred)))))

(define-condition unpermitted-keys ()
  ((keys :initarg :keys))
  (:report (lambda (condition stream)
             (format stream "Unpermitted keys: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(defun condition-continue (condition)
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defmacro with-validation (&body body)
  (let ((missing (gensym "MISSING"))
        (invalid (gensym "INVALID"))
        (unpermitted (gensym "UNPERMITTED")))
    `(let (,missing ,invalid ,unpermitted)
       (handler-bind ((missing-required-keys
                        (lambda (c)
                          (setf ,missing
                                (nconc ,missing (slot-value c 'keys)))
                          (condition-continue c)))
                      (not-satisfied-key
                        (lambda (c)
                          (pushnew (slot-value c 'key) ,invalid)
                          (condition-continue c)))
                      (unpermitted-keys
                        (lambda (c)
                          (setf ,unpermitted
                                (nconc ,unpermitted (slot-value c 'keys)))
                          (condition-continue c))))
         (multiple-value-prog1 (progn ,@body)
           (when (or ,missing ,invalid ,unpermitted)
             (error 'validation-error
                    :missing ,missing
                    :invalid ,invalid
                    :unpermitted ,unpermitted)))))))
