(in-package :cl-user)
(defpackage sanitized-params
  (:use :cl)
  (:export #:alist
           #:requires
           #:permits
           #:satisfies
           #:list-of

           #:sanitize

           #:validation-error
           #:missing-required-keys
           #:unpermitted-keys))
(in-package :sanitized-params)

(defparameter *raise-unpermitted-keys* t)

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

(defun aget (params key)
  (handler-case
      (assoc key params :test #'string=)
    (type-error () nil)))

(defun list-of (pred)
  (lambda (params)
    (block main
      (unless (listp params)
        (return-from main (values nil nil)))

      (handler-case
          (loop for param in params
                append (multiple-value-call
                           (lambda (ok &optional (res param))
                             (when ok
                               (list res)))
                         (funcall pred param))
                  into results
                finally
                   (return (values t results)))
        (type-error ()
          (values nil nil))))))

(defun remove-from-alist (alist key)
  (loop for kv in alist
        unless (equal (car kv) key)
          collect kv))

(defmacro alist (&rest preds)
  (let ((params (gensym "PARAMS"))
        (new-params (gensym "NEW-PARAMS"))
        (permits-all (gensym "PERMITS-ALL")))
    `(lambda (,params)
       (let ((,new-params '())
             (,permits-all t))
         (labels ((collect (key)
                    (let ((kv (aget ,params key)))
                      (if kv
                          (progn
                            (push kv ,new-params)
                            (setf ,params
                                  (remove-from-alist ,params key))
                            t)
                          nil)))
                  (requires (&rest keys)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (let ((missing-keys '()))
                        (dolist (key keys)
                          (or (collect key)
                              ;; Check if the key is already collected
                              (aget ,new-params key)
                              (push key missing-keys)))
                        (when missing-keys
                          (error 'missing-required-keys
                                 :keys missing-keys)))
                      t))
                  (permits (&rest keys)
                    (setf ,permits-all nil)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (map nil #'collect keys)
                      t))
                  (satisfies (key pred)
                    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                    (lambda (&rest args)
                      (declare (ignore args))
                      (let ((kv (aget ,params key)))
                        ;; Ignore the 'satisfies' rule if the key doesn't exist
                        (when kv
                          (multiple-value-call
                              (lambda (ok &optional (res nil res-got-p))
                                ;; Just ignore the key if it does not satisfy.
                                (if ok
                                    (progn
                                      (when res-got-p
                                        (rplacd kv res))
                                      (collect key))
                                    (setf ,params (remove-from-alist ,params key))))
                            (funcall pred (cdr kv)))))
                      t)))
           (if (and (listp ,params)
                    (every #'consp ,params)
                    ,@(loop for pred in preds
                            collect `(funcall ,pred ,params)))
               (progn
                 (setf ,new-params (nreverse ,new-params))
                 (if ,permits-all
                     (setf ,new-params (nconc ,new-params ,params))
                     (when ,params
                       (error 'unpermitted-keys :keys (mapcar #'car ,params))))
                 (values t ,new-params))
               (values nil nil)))))))

(defun sanitize (pattern params)
  (multiple-value-call
      (lambda (ok &optional (res params))
        (when ok
          res))
    (funcall pattern params)))
