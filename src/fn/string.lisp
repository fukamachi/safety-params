(defpackage #:safety-params/fn/string
  (:use #:cl)
  (:import-from #:cl-ppcre)
  (:import-from #:alexandria
                #:named-lambda)
  (:export #:email-string-p))
(in-package #:safety-params/fn/string)

(defun email-string-p (value)
  (and (stringp value)
       (ppcre:scan "\\A[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\.[a-zA-Z0-9-]+)*\\z" value)
       t))

(defun tel-string-p (value)
  (and (stringp value)
       (<= (length value) 15)
       (ppcre:scan "\\A0[0-9]{1,3}-?[0-9]{2,4}-?[0-9]{3,4}\\z" value)
       t))

(defun iso-timestring-p (value)
  (and (stringp value)
       (ppcre:scan "\\A\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2}(?:\\.\\d+)?Z\\z" value)
       t))

(defmacro string-shorter-than (length)
  (let ((v (gensym "V")))
    `(named-lambda ,(intern (format nil "~A-~A-~A" :string-shorter-than length :p)) (,v)
       (and (stringp ,v)
            (<= (length ,v) ,length)))))
