(defpackage #:sanitized-params/tests/main
  (:use #:cl
        #:sanitized-params
        #:rove))
(in-package #:sanitized-params/tests/main)

(deftest list-of-testing
  (testing "list-of"
    (let ((pattern (list-of #'integerp)))
      (ok (equalp (multiple-value-list (funcall pattern 1))
                  '(nil nil))
          "non list value for list-of")
      (ok (equalp (multiple-value-list (funcall pattern '()))
                  '(t nil))
          "nil for list-of")
      (ok (equalp (multiple-value-list (funcall pattern (cons 1 2)))
                  '(nil nil))
          "Can handle TYPE-ERROR")
      (ok (equalp (multiple-value-list (funcall pattern '(nil)))
                  '(t nil))
          "Returns T even if no values are matched")
      (ok (equalp (multiple-value-list (funcall pattern '(1 nil 2 "a" 3)))
                  '(t (1 2 3)))
          "Can only collect integers"))))

(deftest alist-permits-testing
  (testing "alist (check invalid values)"
    (let ((pattern (alist (permits "name"))))
      (ok (equalp (multiple-value-list (funcall pattern 1))
                  '(nil nil)))
      (ok (equalp (multiple-value-list (funcall pattern '()))
                  '(t nil)))
      (ok (equalp (multiple-value-list (funcall pattern '(1)))
                  '(nil nil)))
      (ok (equalp (multiple-value-list (funcall pattern (cons 1 2)))
                  '(nil nil)))))

  (testing "alist (permits)"
    (let ((pattern (alist (permits "name"))))
      (handler-case (progn
                      (funcall pattern '(("address" . "Japan")))
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (unpermitted-keys e) '("address")))))
      (handler-case (progn
                      (funcall pattern '(("name" . "Eitaro") ("address" . "Japan")))
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (unpermitted-keys e) '("address")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name"))))
                  '(t (("name")))))
      (ok (equalp (multiple-value-list (funcall pattern '()))
                  '(t ()))))
    (let ((pattern (alist (permits))))
      (ok (equalp (multiple-value-list (funcall pattern '()))
                  '(t ())))
      (handler-case (progn
                      (funcall pattern '(("name" . "Eitaro")))
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (unpermitted-keys e) '("name"))))))))

(deftest alist-no-preds-testing
  (testing "alist (no preds)"
    (ok (equalp (multiple-value-list (funcall (alist) '(("address" . "Japan"))))
                '(t (("address" . "Japan")))))
    (ok (equalp (multiple-value-list (funcall (alist) ()))
                '(t ())))
    (ok (equalp (multiple-value-list (funcall (alist) 1))
                '(nil nil)))
    (ok (equalp (multiple-value-list (funcall (alist) '(1)))
                '(nil nil)))))

(deftest alist-requires-testing
  (testing "alist (requires)"
    (let ((pattern (alist (requires "name"))))
      (handler-case (progn
                      (funcall pattern '())
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (missing-keys e) '("name")))))
      (handler-case (progn
                      (funcall pattern '(("address" . "Japan")))
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (missing-keys e) '("name")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro"))))
                  '(t (("name" . "Eitaro")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro") ("address" . "Japan"))))
                  '(t (("name" . "Eitaro") ("address" . "Japan"))))
          "Permits all other keys"))))

(deftest alist-satisfies-testing
  (testing "alist (satisfies)"
    (let ((pattern (alist (satisfies "email" #'listp))))
      (ok (equalp (multiple-value-list (funcall pattern '(("email"))))
                  '(t (("email")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro"))))
                  '(t (("name" . "Eitaro")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("email" . "e.arrows@gmail.com"))))
                  '(t nil)))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro") ("email" . ("e.arrows@gmail.com" "another@gmail.com")))))
                  '(t (("email" . ("e.arrows@gmail.com" "another@gmail.com")) ("name" . "Eitaro"))))))))

(deftest alist-satisfies!-testing
  (testing "alist (satisfies!)"
    (let ((pattern (alist (satisfies! "email" #'listp))))
      (ok (equalp (multiple-value-list (funcall pattern '(("email"))))
                  '(t (("email")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro"))))
                  '(t (("name" . "Eitaro")))))
      (handler-case (progn
                      (funcall pattern '(("email" . "e.arrows@gmail.com")))
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (invalid-keys e) '("email")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro") ("email" . ("e.arrows@gmail.com" "another@gmail.com")))))
                  '(t (("email" . ("e.arrows@gmail.com" "another@gmail.com")) ("name" . "Eitaro"))))))))

(deftest complicated-example
  (testing "complicated example"
    (ok (equalp (sanitize
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
                   (("name" . "Masatoshi Sano") ("family") ("hobbies" "rocket" "lisp"))))))))

(deftest initargs-of-testing
  (testing "initargs-of"
    (defclass person ()
      ((name :initarg :name)
       (email :initarg :email)))

    (ok (signals (initargs-of 1)))
    (ok (signals (initargs-of 'integer)))
    (ok (equalp (initargs-of 'person) '("name" "email")))))
