(defpackage #:safety-params/errors
  (:use #:cl
        #:safety-params/utils)
  (:export #:validation-message
           #:safety-params-error
           #:conversion-failed
           #:assertion-failed
           #:invalid-input
           #:validation-error
           #:unpermitted-keys
           #:missing-required-keys
           #:not-satisfied-key
           #:missing-keys
           #:invalid-keys
           #:unpermitted-keys
           #:with-continuable
           #:ignore-and-continue))
(in-package #:safety-params/errors)

(defvar *validation-message* (make-hash-table))

(defun validation-message (pred)
  (let ((pred (etypecase pred
                (symbol (symbol-function pred))
                (function pred))))
    (gethash pred *validation-message*)))

(defun (setf validation-message) (message pred)
  (let ((pred (etypecase pred
                (symbol (symbol-function pred))
                (function pred))))
    (setf (gethash pred *validation-message*) message)))

(define-condition safety-params-error (error) ())

(define-condition conversion-failed (safety-params-error)
  ((value :initarg :value)
   (expected :initarg :expected)))

(define-condition assertion-failed (safety-params-error)
  ((value :initarg :value)
   (test :initarg :test))
  (:report (lambda (condition stream)
             (with-slots (value test) condition
               (format stream "Assertion ~S for ~S failed"
                       test value)))))

(define-condition invalid-input (assertion-failed)
  ((value :initarg :value))
  (:report (lambda (condition stream)
             (format stream "Invalid input: ~S" (slot-value condition 'value)))))

(define-condition unpermitted-keys (safety-params-error)
  ((keys :initarg :keys
         :accessor unpermitted-keys-keys))
  (:report (lambda (condition stream)
             (format stream "Unpermitted keys: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition missing-required-keys (safety-params-error)
  ((keys :initarg :keys
         :accessor missing-required-keys-keys))
  (:report (lambda (condition stream)
             (format stream "Required keys are missing: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition not-satisfied-key (safety-params-error)
  ((key :initarg :key
        :reader not-satisfied-key-key)
   (pred :initarg :pred
         :reader not-satisfied-key-pred)
   (message :initarg :message
            :initform nil))
  (:report (lambda (condition stream)
             (princ (not-satisfied-key-message condition) stream))))

(defun not-satisfied-key-message (error)
  (or (slot-value error 'message)
      (and (lambda-function-p (not-satisfied-key-pred error))
           (documentation (not-satisfied-key-pred error) 'function))
      (validation-message (not-satisfied-key-pred error))))

(define-condition validation-error (safety-params-error)
  ((missing :initarg :missing
            :initform '()
            :accessor missing-keys)
   (invalid :initarg :invalid
            :initform '()
            :accessor invalid-keys)
   (unpermitted :initarg :unpermitted
                :initform '()
                :accessor unpermitted-keys))
  (:report (lambda (condition stream)
             (with-slots (missing invalid unpermitted) condition
               (format stream "Validation errors:")
               (when missing
                 (format stream "~%  Missing: ~{~A~^, ~}" missing))
               (when unpermitted
                 (format stream "~%  Unpermitted: ~{~A~^, ~}" unpermitted))
               (when invalid
                 (format stream "~%  Invalid:")
                 (loop for (key . message) in invalid
                       do (format stream "~%    - ~A~@[ (~A)~]" key message)))))))

(defmacro with-continuable (&body body)
  `(restart-case (progn ,@body)
     (ignore-and-continue ()
       :report "Ignore and continue.")))
