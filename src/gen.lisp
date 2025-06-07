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

(defparameter +skip-space+ (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab)))))

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

(defun element->rule (el)
  (make-rule :rule (->> (x:content el) string->simple p-rule)
             :cat (->> (x:element-metadata el)
                       (gethash "count")
                       (string->keyword))))

(defun p-rule (s)
  (p:parse (<* #'p-condition +skip-space+ #'p-example) s))

#+nil
(p-rule "n = 1 @integer")

(defun p-example (offset)
  (funcall (*> (p:char #\@)
               (p:consume #'identity))
           offset))

(defun p-condition (offset)
  (p:fmap (lambda (list)
            (if (null (cdr list))
                (car list)
                (cons :or list)))
          (funcall (p:sep1 (*> +skip-space+
                               (p:string "or")
                               +skip-space+)
                           #'p-and-condition)
                   offset)))

#+nil
(p:parse #'p-condition "i = 1")
#+nil
(p:parse #'p-condition "i = 1 or v = 2")
#+nil
(p:parse #'p-condition "i = 1 and n = 3 or v = 2")

(defun p-and-condition (offset)
  (p:fmap (lambda (list)
            (if (null (cdr list))
                (car list)
                (cons :and list)))
          (funcall (p:sep1 (*> +skip-space+
                               (p:string "and")
                               +skip-space+)
                           #'p-relation)
                   offset)))

#+nil
(p:parse #'p-and-condition "i = 1")
#+nil
(p:parse #'p-and-condition "i = 1 and v = 0")

(defun p-relation (offset)
  (p:fmap (lambda (list)
            (destructuring-bind (lhs op rhs) list
              (list op lhs rhs)))
          (funcall (<*> #'p-expr
                        (*> +skip-space+ (p:alt (<$ :eq  (p:char #\=))
                                                (<$ :neq (p:string "!="))))
                        (*> +skip-space+ #'p-range-list))
                   offset)))

#+nil
(p:parse #'p-relation "i = 1")
#+nil
(p:parse #'p-relation "i = 1,2")

(defun p-expr (offset)
  (p:fmap (lambda (pair)
            (if (cadr pair)
                (destructuring-bind (op (f v)) pair
                  (list f op v))
                (car pair)))
          (funcall (<*> #'p-operand
                        (*> +skip-space+
                            (p:opt (<*> (<$ :mod (p:char #\%))
                                        (*> +skip-space+ #'p:unsigned)))))
                   offset)))

#+nil
(p:parse #'p-expr "i % 10")
#+nil
(p:parse #'p-expr "i")

(defun p-operand (offset)
  (funcall (p:alt (<$ :n (p:char #\n))
                  (<$ :i (p:char #\i))
                  (<$ :v (p:char #\v))
                  (<$ :w (p:char #\w))
                  (<$ :f (p:char #\f))
                  (<$ :t (p:char #\t))
                  (<$ :c (p:char #\c))
                  (<$ :e (p:char #\e)))
           offset))

#+nil
(p:parse #'p-operand "n = 1")

(defun p-range-list (offset)
  (p:fmap (lambda (list)
            (if (null (cdr list))
                (car list)
                list))
          (funcall (p:sep1 (p:char #\,) (p:alt #'p-range #'p:unsigned))
                   offset)))

#+nil
(p:parse #'p-range-list "17")
#+nil
(p:parse #'p-range-list "0..1,3..5")

(defun p-range (offset)
  (p:fmap (lambda (pair) (cons :range pair))
          (funcall (<*> #'p:unsigned
                        (*> (p:string "..")
                            #'p:unsigned))
                   offset)))

#+nil
(p:parse #'p-range "0..1")

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
