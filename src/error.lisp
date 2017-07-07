(in-package :cl-user)
(defpackage sanitized-params.error
  (:use #:cl)
  (:export :validation-error
           :missing-required-keys
           #:not-satisfied-key
           :unpermitted-keys))
(in-package :sanitized-params.error)

(define-condition validation-error (error) ())

(define-condition missing-required-keys (validation-error)
  ((keys :initarg :keys))
  (:report (lambda (condition stream)
             (format stream "Required keys are missing: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition not-satisfied-key (validation-error)
  ((key :initarg :key)
   (pred :initarg :pred))
  (:report (lambda (condition stream)
             (with-slots (key pred) condition
               (format stream "Key '~A' doesn't satisfy ~A"
                       key pred)))))

(define-condition unpermitted-keys (validation-error)
  ((keys :initarg :keys))
  (:report (lambda (condition stream)
             (format stream "Unpermitted keys: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))
