(defpackage #:safety-params/errors
  (:use #:cl)
  (:export #:safety-params-error
           #:ignorable-error
           #:dangerous-error
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

(define-condition safety-params-error (error) ())

(define-condition ignorable-error (safety-params-error) ())
(define-condition dangerous-error (safety-params-error) ())

(define-condition conversion-failed (ignorable-error)
  ((value :initarg :value)
   (expected :initarg :expected)))

(define-condition assertion-failed (ignorable-error) ())

(define-condition invalid-input (ignorable-error)
  ((value :initarg :value))
  (:report (lambda (condition stream)
             (format stream "Invalid input: ~S" (slot-value condition 'value)))))

(define-condition unpermitted-keys (dangerous-error)
  ((keys :initarg :keys
         :accessor unpermitted-keys-keys))
  (:report (lambda (condition stream)
             (format stream "Unpermitted keys: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition missing-required-keys (dangerous-error)
  ((keys :initarg :keys
         :accessor missing-required-keys-keys))
  (:report (lambda (condition stream)
             (format stream "Required keys are missing: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition not-satisfied-key (ignorable-error)
  ((key :initarg :key
        :accessor not-satisfied-key-key)
   (pred :initarg :pred
         :accessor not-satisfied-key-pred))
  (:report (lambda (condition stream)
             (with-slots (key pred) condition
               (format stream "Key '~A' doesn't satisfy ~A"
                       key pref)))))

(define-condition validation-error (dangerous-error)
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
               (format stream "Validation errors:~@[~%  Missing: ~{~A~^, ~}~]~@[~%  Invalid: ~{~A~^, ~}~]~@[~%  Unpermitted: ~{~A~^, ~}~]"
                       missing invalid unpermitted)))))

(defmacro with-continuable (&body body)
  `(restart-case (progn ,@body)
     (ignore-and-continue ()
       :report "Ignore and continue.")))
