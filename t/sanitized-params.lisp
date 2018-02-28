(in-package :cl-user)
(defpackage sanitized-params-test
  (:use :cl
        :sanitized-params
        :prove))
(in-package :sanitized-params-test)

(plan 9)

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
              'unpermitted-keys)))

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
    (handler-case (progn
                    (funcall pattern '())
                    (fail "Expected to raise VALIDATION-ERROR"))
      (validation-error (e)
        (is (missing-keys e) '("name"))))
    (handler-case (progn
                    (funcall pattern '(("address" . "Japan")))
                    (fail "Expected to raise VALIDATION-ERROR"))
      (validation-error (e)
        (is (missing-keys e) '("name"))))
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

(subtest "alist (satisfies!)"
  (let ((pattern (alist (satisfies! "email" #'listp))))
    (is-values (funcall pattern '(("email")))
               '(t (("email"))))
    (is-values (funcall pattern '(("name" . "Eitaro")))
               '(t (("name" . "Eitaro"))))
    (handler-case (progn
                    (funcall pattern '(("email" . "e.arrows@gmail.com")))
                    (fail "Expected to raise VALIDATION-ERROR"))
      (validation-error (e)
        (is (invalid-keys e) '("email"))))
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
