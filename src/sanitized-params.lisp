(in-package :cl-user)
(defpackage sanitized-params
  (:use #:cl
        #:sanitized-params.error)
  (:import-from #:alexandria
                #:flatten
                #:with-gensyms
                #:when-let)
  (:export #:alist
           #:requires
           #:permits
           #:satisfies
           #:list-of
           #:initargs-of

           #:sanitize

           #:validation-error
           #:missing-required-keys
           #:not-satisfied-key
           #:unpermitted-keys))
(in-package :sanitized-params)

(defparameter *raise-unpermitted-keys* t)

(defun aget (params key)
  (assoc key params :test #'string=))

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

(defun initargs-of (class)
  (let ((class (if (typep class 'standard-class)
                   class
                   (find-class class))))
    (check-type class standard-class)
    (c2mop:ensure-finalized class)
    (loop for slot in (c2mop:class-slots class)
          append (mapcar #'string-downcase
                         (c2mop:slot-definition-initargs slot)))))

(defun remove-from-alist (alist key)
  (loop for kv in alist
        unless (equal (car kv) key)
          collect kv))

(defmacro alist (&rest preds)
  (with-gensyms (params new-params permits-all)
    `(lambda (,params)
       (let ((,new-params '())
             (,permits-all t))
         (labels ((collect (key)
                    (when-let (kv (aget ,params key))
                      (push kv ,new-params)
                      (setf ,params
                            (remove-from-alist ,params key))
                      t))
                  (requires (&rest keys)
                    (setf keys (flatten keys))
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
                    (setf keys (flatten keys))
                    (setf ,permits-all nil)
                    (lambda (&rest args)
                      (declare (ignore args))
                      (loop for key in keys
                            while ,params
                            do (collect key))
                      t))
                  (satisfies (key pred)
                    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                    (lambda (&rest args)
                      (declare (ignore args))
                      ;; Ignore the 'satisfies' rule if the key doesn't exist
                      (when-let (kv (aget ,params key))
                        (multiple-value-call
                            (lambda (ok &optional (res nil res-got-p))
                              (if ok
                                  (progn
                                    (when res-got-p
                                      (rplacd kv res))
                                    (collect key))
                                  (error 'not-satisfied-key
                                         :key key
                                         :pred pred)))
                          (funcall pred (cdr kv))))
                      t)))
           (if (and (listp ,params)
                    (handler-case
                        (every #'consp ,params)
                      (type-error () nil))
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
