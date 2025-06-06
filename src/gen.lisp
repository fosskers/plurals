(defpackage cldr-plurals/gen
  (:use :cl :arrow-macros)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:x #:parcom/xml)
                    (#:t #:transducers)
                    (#:p #:parcom)))

(in-package :cldr-plurals/gen)

;; CONCLUSION: None of the `other' categories have actual rules. If a given
;; number doesn't match any of the actual concrete rules, it's fine for it to
;; default to the `other' line of the translation (or the line marked as default
;; if `other' is missing).

(defun string->keyword (s)
  (intern (string-upcase s) "KEYWORD"))

(declaim (ftype (function (string) (simple-array character (*))) string-simple))
(defun string->simple (s)
  "Ensure that we have an owned copy of some string, enabling us to later parse it
with `parcom'."
  (subseq s 0))

#+nil
(let* ((s "hello there")
       (z (make-array 3 :element-type 'character :displaced-index-offset 2 :displaced-to s)))
  (string->simple z))

(defstruct rule
  (cat  nil :type keyword)
  (rule nil :type string))

;; TODO: Start here. The string we're attempting to parse isn't a simple-string.
(defun element->rule (el)
  (make-rule :rule (->> (x:content el) string->simple parse-rule)
             :cat (->> (x:element-metadata el)
                       (gethash "count")
                       (string->keyword))))

(defun parse-rule (s)
  (p:parse (p:take-while (lambda (c) (not (eql c #\@)))) s))

(defun rules-by-locale (xml)
  "Parse some XML and recollate all plural rules into a single Hash Table, keyed by
the locale."
  (->> (x:parse xml)
       (x:content)
       (gethash "plurals")
       (x:content)
       (gethash "pluralRules")
       (t:transduce (t:comp (t:map (lambda (el)
                                     (let ((rules (->> (x:content el)
                                                       (gethash "pluralRule")
                                                       (funcall (lambda (x)
                                                                  (if (listp x)
                                                                      (mapcar #'element->rule x)
                                                                      (list (element->rule x)))))))
                                           (locales (->> (x:element-metadata el)
                                                         (gethash "locales")
                                                         (t::string-split)
                                                         (mapcar #'string->keyword)))
                                           (ht (make-hash-table :test #'eq)))
                                       (dolist (locale locales)
                                         (setf (gethash locale ht) rules))
                                       ht))))
                    (t:fold (lambda (acc ht) (merge-hash-tables! #'append acc ht))))))

#+nil
(rules-by-locale (uiop:read-file-string #p"plurals.xml"))

(defun merge-hash-tables! (f a b)
  "Merge all of the elements of B into A, fusing their values via some F if both
tables contain the same key."
  (maphash (lambda (k v)
             (let ((x (gethash k a)))
               (if x
                   (setf (gethash k a) (funcall f x v))
                   (setf (gethash k a) v))))
           b)
  a)

#+nil
(let ((a (make-hash-table :test #'eq))
      (b (make-hash-table :test #'eq)))
  (setf (gethash :a a) 1)
  (setf (gethash :b a) 2)
  (setf (gethash :c a) 3)
  (setf (gethash :a b) 10)
  (setf (gethash :b b) 20)
  (setf (gethash :c b) 30)
  (setf (gethash :d b) 99)
  (merge-hash-tables! #'+ a b))
