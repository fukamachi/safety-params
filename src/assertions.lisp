(defpackage #:safety-params/assertions
  (:use #:cl
        #:safety-params/errors)
  (:import-from #:safety-params/errors
                #:unpermitted-keys-keys
                #:not-satisfied-key-key
                #:missing-required-keys-keys)
  (:import-from #:safety-params/conversions
                #:conversion
                #:conversion-fn
                #:conversion-default)
  (:import-from #:alexandria
                #:with-gensyms
                #:nconcf)
  (:export #:list-of
           #:alist
           #:satisfies
           #:permits
           #:requires))
(in-package #:safety-params/assertions)

(defun list-of (pred)
  (check-type pred (or function symbol conversion))
  (lambda (param)
    (block nil
      (unless (typep param 'list)
        (with-continuable
          (error 'assertion-failed)))
      (values
       t
       (typecase pred
         (conversion
          (loop with results
                for value in param
                do (with-continuable
                     (handler-case (push (funcall (conversion-fn pred) value) results)
                       (type-error ()
                         (error 'assertion-failed))))
                finally (return (nreverse results))))
         (otherwise
          (let ((satisfied (remove-if-not
                            (lambda (param)
                              (with-continuable
                                (handler-case
                                    (funcall pred param)
                                  (type-error ()
                                    (error 'assertion-failed)))))
                            param)))
            (unless (equalp param satisfied)
              (with-continuable
                (error 'assertion-failed)))
            satisfied)))))))

(defvar *params*)

(defun %satisfies (key pred)
  (let ((val (assoc key *params* :test #'equal)))
    (typecase pred
      (conversion
       (handler-case
           (cond
             (val
              (cdr (rplacd val (funcall (conversion-fn pred) (cdr val)))))
             ((conversion-default pred)
              (push (cons key (conversion-default pred)) *params*)))
         (conversion-failed ()
           (error 'not-satisfied-key
                  :key key
                  :pred pred))))
      (otherwise
       (when val
         (multiple-value-call
             (lambda (result &optional (new-val (cdr val)))
               (if result
                   (progn
                     (rplacd val new-val)
                     (values t new-val))
                   (error 'not-satisfied-key
                          :key key
                          :pred pred)))
           (funcall pred (cdr val))))))))

(defun %requires (&rest keys)
  (let ((not-exists (remove-if (lambda (key)
                                 (assoc key *params* :test #'equal))
                               keys)))
    (when not-exists
      (error 'missing-required-keys
             :keys not-exists))
    t))

(defun %permits (&rest keys)
  (let ((unpermitted (remove-if (lambda (param)
                                  (find param keys :test #'equal))
                                *params*
                                :key #'car)))
    (when unpermitted
      (error 'unpermitted-keys
             :keys (mapcar #'car unpermitted)))
    t))

(defun invoke-continue (e)
  (let ((restart (find-restart 'ignore-and-continue e)))
    (when restart
      (invoke-restart restart))))

(defmacro alist (&rest preds)
  (with-gensyms (params key keys pred missing invalid unpermitted e)
    `(lambda (,params)
       (block nil
         (unless (and (listp ,params)
                      (every #'consp ,params))
           (restart-case
               (error 'assertion-failed)
             (ignore-and-continue ()
               :report "Ignore and continue."
               (return (values nil nil)))))
         (let ((*params* ,params)
               ,missing ,invalid ,unpermitted)
           (flet ((satisfies (,key ,pred)
                    (%satisfies ,key ,pred))
                  (requires (&rest ,keys)
                    (apply #'%requires ,keys))
                  (permits (&rest ,keys)
                    (apply #'%permits ,keys)))
             (handler-bind ((unpermitted-keys
                              (lambda (,e)
                                (nconcf ,unpermitted
                                        (unpermitted-keys-keys ,e))
                                (invoke-continue ,e)))
                            (missing-required-keys
                              (lambda (,e)
                                (nconcf ,missing
                                        (missing-required-keys-keys ,e))
                                (invoke-continue ,e)))
                            (not-satisfied-key
                              (lambda (,e)
                                (pushnew (not-satisfied-key-key ,e) ,invalid)
                                (invoke-continue ,e))))
               (restart-case (progn ,@preds)
                 (ignore-and-continue ()
                   :report "Ignore and continue.")))
             (when (or ,unpermitted
                       ,missing
                       ,invalid)
               (error 'validation-error
                      :missing ,missing
                      :invalid ,invalid
                      :unpermitted ,unpermitted))
             (values t *params*)))))))