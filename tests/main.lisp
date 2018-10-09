(defpackage #:safety-params/tests/main
  (:use #:cl
        #:safety-params
        #:rove))
(in-package #:safety-params/tests/main)

(deftest converts-into-tests
  (testing "string"
    (ok (= (converts-into "10" 'integer) 10))))

(deftest list-of-tests
  (testing "list-of normal functions"
    (ok (signals (funcall (list-of #'integerp) 1)
                 'assertion-failed))
    (ok (equalp (multiple-value-list (funcall (list-of #'integerp) '()))
                '(t nil)))
    (ok (signals (funcall (list-of #'integerp) (cons 1 2))
                 'assertion-failed))
    (ok (equalp (multiple-value-list (funcall (list-of #'integerp)
                                              '(1 2 3)))
                '(t (1 2 3))))
    (ok (signals (funcall (list-of #'integerp)
                          '(1 2 "10" 3))
                 'assertion-failed))
    (ok (equalp (multiple-value-list (funcall (list-of (being integer))
                                              '(1 2 "10" 3)))
                '(t (1 2 10 3))))
    (ok (signals (funcall (list-of (being integer))
                          '(1 2 "a" 3))
                 'conversion-failed))))

(deftest alist-tests
  (testing "permits (invalid input)"
    (let ((pattern (alist (permits "name"))))
      (ok (signals (funcall pattern '(("address" . "Japan")))
                   'validation-error))
      (ok (equalp (multiple-value-list (funcall pattern '()))
                  '(t ())))
      (ok (signals (funcall pattern '(1))
                   'assertion-failed))
      (ok (signals (funcall pattern (cons 1 2))
                   'assertion-failed))))

  (testing "permits"
    (let ((pattern (alist (permits "name"))))
      (handler-case (progn
                      (funcall pattern '(("address" . "Japan")))
                      (fail "Expected to raise VALIDATION-ERROR"))
        (validation-error (e)
          (ok (equalp (unpermitted-keys e) '("address")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . nil))))
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
          (ok (equalp (unpermitted-keys e) '("name")))))))

  (testing "no preds"
    (ok (equalp (multiple-value-list (funcall (alist) '(("address" . "Japan"))))
                '(t (("address" . "Japan")))))
    (ok (equalp (multiple-value-list (funcall (alist) ()))
                '(t ())))
    (ok (signals (funcall (alist) 1)
                 'assertion-failed))
    (ok (signals (funcall (alist) '(1))
                 'assertion-failed)))

  (testing "requires"
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
      (ok (equalp
           (multiple-value-list
            (funcall pattern '(("name" . "Eitaro")
                               ("address" . "Japan"))))
           '(t (("name" . "Eitaro") ("address" . "Japan")))))))

  (testing "satisfies"
    (let ((pattern (alist (satisfies "email" #'listp))))
      (ok (equalp (multiple-value-list (funcall pattern '(("email"))))
                  '(t (("email")))))
      (ok (equalp (multiple-value-list (funcall pattern '(("name" . "Eitaro"))))
                  '(t (("name" . "Eitaro")))))
      (ok (signals (funcall pattern '(("email" . "e.arrows@gmail.com")))
                   'validation-error))
      (ok (equalp (multiple-value-list
                   (funcall pattern '(("name" . "Eitaro")
                                      ("email" . ("e.arrows@gmail.com" "another@gmail.com")))))
                  '(t (("name" . "Eitaro")
                       ("email" . ("e.arrows@gmail.com" "another@gmail.com"))))))))

  (testing "complicated example"
    (ok (equalp (multiple-value-list
                 (funcall
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
                                   ("hobbies" . ("rocket" "lisp"))))))))
                '(t (("name" . "Eitaro Fukamachi")
                     ("email" "e.arrows@gmail.com" "another@gmail.com")
                     ("friends"
                      (("name" . "Masatoshi Sano") ("family") ("hobbies" "rocket" "lisp")))))))))
