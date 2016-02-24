# Sanitized-Params

Sanitized-Params is for checking required keys and filtering invalid key-values in an association list.

## Usage

```common-lisp
(use-package :sanitized-params)

;; Check every elements satisfies #'integerp
(sanitize
 (list-of #'integerp)
 '(1 2 3))
;=> (1 2 3)

;; Return only satisfied values
(sanitize
 (list-of #'integerp)
 '(1 2 "a" 3))
;=> (1 2 3)

;; Check the association list has a key "name"
(sanitize
 (alist (requires "name"))
 '(("name" . "Eitaro Fukamachi")))
;=> '(("name" . "Eitaro Fukamachi"))

(sanitize
 (alist (requires "name"))
 '(("name" . "Eitaro Fukamachi")
   ("address" . "Japan")))
;=> (("name" . "Eitaro Fukamachi") ("address" . "Japan"))

;; Whitelist keys
(sanitize
 (alist (permits "name" "email"))
 '(("name" . "Eitaro Fukamachi")))
;=> (("name" . "Eitaro Fukamachi"))

(sanitize
 (alist (permits "name" "email"))
 '(("name" . "Eitaro Fukamachi") ("address" . "Japan")))
;-> ERROR: Unpermitted keys: "address

;; Nested rule
(sanitize
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
;=> (("name" . "Eitaro Fukamachi")
;    ("email" "e.arrows@gmail.com" "another@gmail.com")
;    ("friends"
;     (("name" . "Masatoshi Sano") ("family") ("hobbies" "rocket" "lisp"))))
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
