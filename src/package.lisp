;;; After analyzing the actual data, only the following "operands" are in use:
;;;
;;;   N I V F T E
;;;
;;; So, W and C are not actually in use. Note that E is intended to be
;;; deprecated, with C being the real operand.
;;;
;;; E has also been intentionally stunted to always return 0. Why are you trying
;;; to pluralize numbers with exponents in them? Besides, the rules in the spec
;;; have the character `c' as the exponent marker, which programming languages
;;; typically don't do. So having the user conform to that is extra annoying.
;;; Instead, please pre-normalize your numbers before attempting to determine
;;; their plural rule.

(defpackage cldr-plurals
  (:use :cl)
  (:export #:op-n #:op-i #:op-v #:op-f #:op-t #:op-e)
  (:documentation ""))

(in-package :cldr-plurals)

(defun op-n (s)
  "The value of the number itself."
  (let ((n (read-from-string s)))
    n))

#+nil
(op-n "1.30")

(defun op-i (s)
  "The number casted to an integer."
  (let* ((n (read-from-string s))
         (int (floor n)))
    int))

#+nil
(op-i "1.3")

(defun op-v (s)
  "The number of visible fraction digits, with trailing zeros."
  (destructuring-bind (whole &rest fraction) (string-split s :separator #\.)
    (declare (ignore whole))
    (cond ((null fraction) 0)
          (t (length (car fraction))))))

#+nil
(op-v "1.030")

(defun op-f (s)
  "The visible fraction digits, with trailing zeros, as an integer."
  (destructuring-bind (whole &rest fraction) (string-split s :separator #\.)
    (declare (ignore whole))
    (cond ((null fraction) 0)
          (t (let ((n (read-from-string (car fraction))))
               n)))))

#+nil
(op-f "1.0")
#+nil
(op-f "1.30")

(defun op-t (s)
  "The visible fraction digits, without trailing zeros, as an integer."
  (destructuring-bind (whole &rest fraction) (string-split s :separator #\.)
    (declare (ignore whole))
    (cond ((null fraction) 0)
          (t (let ((trimmed (string-right-trim '(#\0) (car fraction))))
               (cond ((zerop (length trimmed)) 0)
                     (t (let ((n (read-from-string trimmed)))
                          n))))))))

#+nil
(op-t "1.30")

(defun op-e (s)
  "Exponent of the power of 10 used in compact decimal formatting. Intentionally
stunted (see module docs)."
  (declare (ignore s))
  0)

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
