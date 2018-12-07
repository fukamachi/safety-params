(defpackage #:safety-params/fn/number
  (:use #:cl)
  (:import-from #:alexandria
                #:positive-integer-p
                #:negative-integer-p
                #:positive-float-p
                #:negative-float-p)
  (:export #:integerp
           #:positive-integer-p
           #:negative-integer-p
           #:integer-string-p
           #:positive-integer-string-p
           #:negative-integer-string-p
           #:number-string-p
           #:positive-number-string-p
           #:negative-number-string-p
           #:positive-float-string-p
           #:negative-float-string-p))
(in-package #:safety-params/fn/number)

(defun integer-char-p (char)
  (and (characterp char)
       (<= (char-code #\0) (char-code char) (char-code #\9))))

(defun integer-string-p (value)
  (and (stringp value)
       (< 0 (length value))
       (let ((start (if (and (or (char= (aref value 0) #\-)
                                 (char= (aref value 0) #\+))
                             (< 1 (length value)))
                        1
                        0)))
         (loop for i from start below (length value)
               unless (integer-char-p (aref value i))
                 do (return nil)
               finally
                  (return t)))))

(defun positive-integer-string-p (value)
  (and (integer-string-p value)
       (< 0 (read-from-string value))))

(defun negative-integer-string-p (value)
  (and (integer-string-p value)
       (< (read-from-string value) 0)))

(defun number-string-p (string)
  (check-type string string)
  (when (zerop (length string))
    (return-from number-string-p nil))
  (let ((start
          (if (or (char= (aref string 0) #\-)
                  (char= (aref string 0) #\+))
              (if (< 1 (length string))
                  1
                  (return-from number-string-p nil))
              0))
        (end (length string)))
    (declare (type integer start))
    (let ((dot-read nil)
          (slash-read nil))
      (do ((i start (1+ i)))
          ((= end i))
        (let ((char (aref string i)))
          (cond
            ((digit-char-p char))
            ((char= char #\.)
             (when (or dot-read
                       slash-read)
               (return-from number-string-p nil))
             (setq dot-read i))
            ((and (char= char #\/)
                  (not (= i start)))
             (when (or dot-read
                       slash-read)
               (return-from number-string-p nil))
             (setq slash-read i))
            (t (return-from number-string-p nil)))))
      (or (not slash-read)
          (and (/= slash-read end)
               (find-if (lambda (v) (char/= v #\0))
                        (subseq string (1+ slash-read)))
               t)))))

(defun positive-number-string-p (value)
  (and (number-string-p value)
       (< 0 (read-from-string value))))

(defun negative-number-string-p (value)
  (and (number-string-p value)
       (< (read-from-string value) 0)))

(defun positive-float-string-p (value)
  (and (number-string-p value)
       (positive-float-p (read-from-string value))))

(defun negative-float-string-p (value)
  (and (number-string-p value)
       (negative-float-p (read-from-string value))))

(defmacro number-greater-than (value)
  )
