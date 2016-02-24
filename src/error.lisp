(in-package :cl-user)
(defpackage sanitized-params.error
  (:use #:cl)
  (:export :validation-error
           :missing-required-keys
           :unpermitted-keys))
(in-package :sanitized-params.error)

(define-condition validation-error (error) ())

(define-condition missing-required-keys (validation-error)
  ((keys :initarg :keys))
  (:report (lambda (condition stream)
             (format stream "Required keys are missing: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))

(define-condition unpermitted-keys (validation-error)
  ((keys :initarg :keys))
  (:report (lambda (condition stream)
             (format stream "Unpermitted keys: ~{~S~^, ~}"
                     (slot-value condition 'keys)))))
