(in-package :cl-user)
(defpackage sanitized-params-test
  (:use :cl
        :sanitized-params
        :prove))
(in-package :sanitized-params-test)

(plan 8)

(defmacro is-check (pred-fn params new-params)
  `(is-values (funcall ,pred-fn ,params)
              (list t ,new-params)
              (format nil "~S =>~A=> ~S"
                      ,params
                      ,(prin1-to-string pred-fn)
                      ,new-params)))

(defmacro isnt-check (pred-fn params)
  `(is-values (funcall ,pred-fn ,params)
              (list nil nil)
              (format nil "~S isn't ~A"
                      ,params
                      ,(prin1-to-string pred-fn))))

(subtest "list-of"
  (let ((pattern (list-of #'integerp)))
    (is-values (funcall pattern 1)
               '(nil nil)
               "non list value for list-of")
    (is-values (funcall pattern '())
               '(t nil)
               "nil for list-of")
    (is-values (funcall pattern (cons 1 2))
               '(nil nil)
               "Can handle TYPE-ERROR")
    (is-values (funcall pattern '(nil))
               '(t nil)
               "Returns T even if no values are matched")
    (is-values (funcall pattern '(1 nil 2 "a" 3))
               '(t (1 2 3))
               "Can only collect integers")))

(subtest "alist (check invalid values)"
  (let ((pattern (alist (permits "name"))))
    (is-values (funcall pattern 1)
               '(nil nil))
    (is-values (funcall pattern '())
               '(t nil))
    (is-values (funcall pattern '(1))
               '(nil nil))
    (is-values (funcall pattern (cons 1 2))
               '(nil nil))))

(subtest "alist (permits)"
  (let ((pattern (alist (permits "name"))))
    (is-error (funcall pattern '(("address" . "Japan")))
              'unpermitted-keys)
    (is-error (funcall pattern '(("name" . "Eitaro") ("address" . "Japan")))
              'unpermitted-keys)
    (is-values (funcall pattern '(("name")))
               '(t (("name"))))
    (is-values (funcall pattern '())
               '(t ())))
  (let ((pattern (alist (permits))))
    (is-values (funcall pattern '())
               '(t ()))
    (is-error (funcall pattern '(("name" . "Eitaro")))
              'unpermitted-keys))
  ;; (is-check (alist (requires "name"))
  ;;           '(("name" . "Eitaro")) '(("name" . "Eitaro")))
  ;; (is-check (alist (requires "name"))
  ;;           '(("name" . "Eitaro") ("address" . "Japan")) '(("name" . "Eitaro")))
  ;; (isnt-check (alist (requires "name"))
  ;;             '(("address" . "Japan")))
  ;; (isnt-check (alist (requires "name")) '())
  ;; (isnt-check (alist (requires "name")) 1)
  )

(subtest "alist (no preds)"
  (is-values (funcall (alist) '(("address" . "Japan")))
             '(t (("address" . "Japan"))))
  (is-values (funcall (alist) ())
             '(t ()))
  (is-values (funcall (alist) 1)
             '(nil nil))
  (is-values (funcall (alist) '(1))
             '(nil nil)))

(subtest "alist (requires)"
  (let ((pattern (alist (requires "name"))))
    (is-error (funcall pattern '())
              'missing-required-keys)
    (is-error (funcall pattern '(("address" . "Japan")))
              'missing-required-keys)
    (is-values (funcall pattern '(("name" . "Eitaro")))
               '(t (("name" . "Eitaro"))))
    (is-values (funcall pattern '(("name" . "Eitaro") ("address" . "Japan")))
               '(t (("name" . "Eitaro") ("address" . "Japan")))
               "Permits all other keys")))

(subtest "alist (satisfies)"
  (let ((pattern (alist (satisfies "email" #'listp))))
    (is-values (funcall pattern '(("email")))
               '(t (("email"))))
    (is-values (funcall pattern '(("name" . "Eitaro")))
               '(t (("name" . "Eitaro"))))
    (is-values (funcall pattern '(("email" . "e.arrows@gmail.com")))
               '(t nil))
    (is-values (funcall pattern '(("name" . "Eitaro") ("email" . ("e.arrows@gmail.com" "another@gmail.com"))))
               '(t (("email" . ("e.arrows@gmail.com" "another@gmail.com")) ("name" . "Eitaro"))))))

(subtest "complicated example"
  (is (sanitize
       (alist
        (requires "name")
        (satisfies "email" #'listp)
        (satisfies "friends"
                   (list-of
                    (alist
                     (requires "name")
                     (satisfies "family" (list-of
                                          (alist (permits "name"))))
                     (satisfies "hobbies" #'listp)))))
       '(("name" . "Eitaro Fukamachi")
         ("email" . ("e.arrows@gmail.com" "another@gmail.com"))
         ("friends" . ((("name" . "Masatoshi Sano")
                        ("family" . ())
                        ("hobbies" . ("rocket" "lisp")))))))
      '(("name" . "Eitaro Fukamachi")
        ("email" "e.arrows@gmail.com" "another@gmail.com")
        ("friends"
         (("name" . "Masatoshi Sano") ("family") ("hobbies" "rocket" "lisp"))))))

(subtest "initargs-of"
  (defclass person ()
    ((name :initarg :name)
     (email :initarg :email)))

  (is-error (initargs-of 1) 'error)
  (is-error (initargs-of 'integer) 'error)
  (is (initargs-of 'person) '("name" "email")))

(finalize)
