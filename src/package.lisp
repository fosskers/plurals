;;; After analyzing the actual data, only the following "operands" are in use:
;;;
;;;   N I V F T E
;;;
;;; So, W and C are not actually in use. Note that E is intended to be
;;; deprecated, with C being the real operand.

(defpackage cldr-plurals
  (:use :cl)
  (:documentation ""))

(in-package :cldr-plurals)

(defun operator-n (s)
  "The value of the number itself."
  (let ((n (read-from-string s)))
    n))

#+nil
(operator-n "1.30")

(defun operator-i (s)
  "The number casted to an integer."
  (let* ((n (read-from-string s))
         (int (floor n)))
    int))

#+nil
(operator-i "1.3")

(defun operator-v (s)
  "The number of visible fraction digits, with trailing zeros."
  (destructuring-bind (whole fraction) (string-split s :separator #\.)
    (declare (ignore whole))
    (length fraction)))

#+nil
(operator-v "1.030")

(defun operator-f (s)
  "The visible fraction digits, with trailing zeros, as an integer."
  (destructuring-bind (whole fraction) (string-split s :separator #\.)
    (declare (ignore whole))
    (let ((n (read-from-string fraction)))
      n)))

#+nil
(operator-f "1.30")

(defun operator-t (s)
  "The visible fraction digits, without trailing zeros, as an integer."
  (destructuring-bind (whole fraction) (string-split s :separator #\.)
    (declare (ignore whole))
    (let* ((trimmed (string-right-trim '(#\0) fraction))
           (n (read-from-string trimmed)))
      n)))

#+nil
(operator-t "1.30")

(defun operator-e (s)
  "Exponent of the power of 10 used in compact decimal formatting."
  (destructuring-bind (rest c) (string-split s :separator #\c)
    (declare (ignore rest))
    (let ((n (read-from-string c)))
      n)))

#+nil
(operator-e "123c6")

;; Borrowed from `transducers'.
(declaim (ftype (function (string &key (:separator character)) list) string-split))
(defun string-split (string &key (separator #\space))
  "Split a string into a list of substrings according to some configurable
separator character."
  (labels ((recurse (acc start end)
             (declare (type fixnum start end))
             (cond ((and (< start 0) (< end 0)) acc)
                   ;; The separator was found at the very start of the string.
                   ((and (zerop start) (eql separator (char string start)))
                    (cl:cons "" (cl:cons (subseq string (1+ start) (1+ end)) acc)))
                   ;; We got to the beginning without seeing another separator.
                   ((zerop start) (cl:cons (subseq string start (1+ end)) acc))
                   ;; Normal separator detection: collect the piece we've built.
                   ((eql separator (char string start))
                    (recurse (cl:cons (subseq string (1+ start) (1+ end)) acc)
                             (1- start)
                             (1- start)))
                   ;; Base case: just keep moving.
                   (t (recurse acc (1- start) end)))))
    ;; We start from the end of the string and go backwards, in order to neatly
    ;; build up the final list without needing to `reverse'.
    (let ((end (1- (length string))))
      (recurse '() end end))))

#+nil
(string-split "1.03" :separator #\.)
