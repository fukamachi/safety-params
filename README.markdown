# Safety-Params

[![Build Status](https://travis-ci.org/fukamachi/sanitized-params.svg?branch=master)](https://travis-ci.org/fukamachi/sanitized-params)
[![Coverage Status](https://coveralls.io/repos/fukamachi/sanitized-params/badge.svg?branch=master)](https://coveralls.io/r/fukamachi/sanitized-params)
[![Quicklisp dist](http://quickdocs.org/badge/sanitized-params.svg)](http://quickdocs.org/sanitized-params/)

Safety-Params is for checking values.

## Upgrading Notes to v0.3.0

This library is used to be called "Sanitized-Params" which is designed to sanitize, which means it doesn't raise any errors and just omits key-values even when the specified condition has failed.

However, it has been commonly used mainly for validating values. That made me decide to revise its APIs and give 2 macros, `sanitize` and `validate`.

Additionally, I found that value-conversions are often following after a validation. For instance, a code expecting an integer but its input could be a string.

Since v0.3.0, the value-conversions features were added:

```common-lisp
(validate
 (alist
  (satisfies "amount" (being integer)))
 '(("amount" . "108")))
;=> (("amount" . 108))  <- Implicit conversion "108" -> 108
```

## Usage

### Checking values

#### List

```common-lisp
(validate
 (list-of #'integerp)
 '(1 2 3))
;=> (1 2 3)

(validate
 (list-of #'integerp)
 '(1 2 "3" 4))
;-> ASSERTION-FAILD: Assertion #<FUNCTION INTEGERP> for (1 2 "3" 4) failed
```

#### Association List

```common-lisp
(defvar *params*
  '(("id" . "4e3c89aa-4244-4014-ae36-dfaf1dc969f5")
    ("amount" . 10)))

;; Validating key-values
(validate
 (alist
  (satisfies "id" #'stringp)
  (satisfies "amount" #'integerp))
 *params*)

;; Checking permitted key-values
(validate
 (alist
  (permits "id" "amount"))
 *params*)

;; Checking required key-values
(validate
 (alist
  (requires "id"))
 *params*)
```

#### Nested example

```common-lisp
(validate
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
;=> (("name" . "Eitaro Fukamachi") ("email" "e.arrows@gmail.com" "another@gmail.com")
;    ("friends" (("name" . "Masatoshi Sano") ("family") ("hobbies" "rocket" "lisp"))))
```

### Converting values

```common-lisp
(validate
 (list-of (being integer))
 '(1 2 "3" 4))
;=> (1 2 3 4)
```

### Sanitizing values

```common-lisp
(sanitize
 (list-of #'integerp)
 '(1 2 "3" 4))
;=> (1 2 4)
```

## Author

* Eitaro Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2016 Eitaro Fukamachi (e.arrows@gmail.com)

## License

Licensed under the BSD 2-Clause License.
