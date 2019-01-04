(defpackage #:safety-params/assertions
  (:use #:cl
        #:safety-params/errors)
  (:import-from #:safety-params/errors
                #:unpermitted-keys-keys
                #:not-satisfied-key-key
                #:not-satisfied-key-message
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
          (error 'invalid-input :value param)))
      (values
       t
       (typecase pred
         (conversion
          (loop with results
                for value in param
                do (with-continuable
                     (handler-case (push (funcall (conversion-fn pred) value) results)
                       (type-error ()
                         (error 'invalid-input :value param))))
                finally (return (nreverse results))))
         (otherwise
          (let ((satisfied (handler-case
                               (remove-if-not
                                (lambda (param)
                                  (with-continuable
                                    (handler-case
                                        (funcall pred param)
                                      (type-error ()
                                        (error 'invalid-input :value param)))))
                                param)
                             (type-error ()
                               (error 'invalid-input :value param)))))
            (unless (equalp param satisfied)
              (with-continuable
                (error 'assertion-failed
                       :test pred
                       :value param)))
            satisfied)))))))

(defvar *params*)

(defun %satisfies (key pred &key message)
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
           (with-continuable
             (error 'not-satisfied-key
                    :key key
                    :pred pred
                    :message message)))))
      (otherwise
       (when val
         (multiple-value-call
             (lambda (result &optional (new-val (cdr val)))
               (if result
                   (progn
                     (rplacd val new-val)
                     (values t new-val))
                   (with-continuable
                     (error 'not-satisfied-key
                            :key key
                            :pred pred
                            :message message))))
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
  (let ((restart (find-restart 'collect-validation-errors e)))
    (when restart
      (invoke-restart restart))))

(defmacro alist (&rest preds)
  (with-gensyms (params key keys pred missing invalid unpermitted e satisfies-args)
    `(lambda (,params)
       (block nil
         (unless (and (listp ,params)
                      (handler-case
                          (every #'consp ,params)
                        (type-error ()
                          (with-continuable
                            (error 'invalid-input :value ,params)))))
           (with-continuable
             (error 'invalid-input :value ,params)))
         (let ((*params* ,params)
               ,missing ,invalid ,unpermitted)
           (flet ((satisfies (,key ,pred &rest ,satisfies-args)
                    #+sbcl (declare (sb-ext:muffle-conditions sb-ext:code-deletion-note))
                    (restart-case
                        (apply #'%satisfies ,key ,pred ,satisfies-args)
                      (collect-validation-errors ())))
                  (requires (&rest ,keys)
                    (restart-case
                        (apply #'%requires ,keys)
                      (collect-validation-errors ())))
                  (permits (&rest ,keys)
                    (restart-case
                        (apply #'%permits ,keys)
                      (collect-validation-errors ()))))
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
                                (pushnew (cons (not-satisfied-key-key ,e)
                                               (not-satisfied-key-message ,e)) ,invalid)
                                (invoke-continue ,e))))
               ,@preds)
             (cond
               (,missing
                (error 'validation-error
                       :missing ,missing
                       :invalid ,invalid
                       :unpermitted ,unpermitted))
               ((or ,unpermitted ,invalid)
                (with-continuable
                  (error 'validation-error
                         :unpermitted ,unpermitted
                         :invalid ,invalid))
                (values nil
                        (remove-if (lambda (,key)
                                     (or (find ,key ,invalid :test 'equal)
                                         (find ,key ,unpermitted :test 'equal)))
                                   *params*
                                   :key #'car)))
               (t
                (values t *params*)))))))))
