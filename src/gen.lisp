(defpackage cldr-plurals/gen
  (:use :cl :arrow-macros)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:x #:parcom/xml)
                    (#:t #:transducers)
                    (#:p #:parcom)))

(in-package :cldr-plurals/gen)

(defstruct rule
  "A temporary housing for a pair of a rule category (`one', etc.) and its actual
parsed rule. Later flattened out into an actual Hash Table key-value pair."
  (cat  nil :type keyword)
  (rule nil :type (or null list)))

(defun element->rule (el)
  "`parcom/xml' types are quite highly nested, so this flattens one out."
  (make-rule :rule (->> (x:content el) string->simple p-rule)
             :cat  (->> (x:element-metadata el)
                        (gethash "count")
                        (string->keyword))))

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
                    (t:fold (lambda (acc ht) (merge-hash-tables! #'append acc ht))))
       (t:transduce (t:comp (t:map (lambda (pair) (cons (car pair) (rule-list->ht (cdr pair))))))
                    #'t:hash-table)))

#+nil
(rules-by-locale (uiop:read-file-string #p"data/plurals.xml"))

#+nil
(rules-by-locale (uiop:read-file-string #p"data/ordinals.xml"))

(defun rule-list->ht (rules)
  "Without this, the rules are just a flat list of `rule' structs, which isn't
useful for later lookups. Note that this also removes the `:other' category, and
those were demonstrated to never contain actual rules."
  (let ((ht (make-hash-table :test #'eq)))
    (dolist (rule rules)
      (setf (gethash (rule-cat rule) ht) (rule-rule rule)))
    (remhash :other ht)
    ht))

;; --- Utilities --- ;;

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

;; The code below proves which "operands" are actually in use in the data,
;; regardless of what the spec says.

#+nil
(->> (uiop:read-file-string #p"data/plurals.xml")
     (rules-by-locale)
     (t:transduce (t:comp (t:map #'cdr)
                          (t:map (lambda (ht) (t:transduce (t:map #'cdr) #'t:cons ht)))
                          #'t:flatten
                          (t:filter #'keywordp)
                          (t:filter (lambda (kw)
                                      (not (or (eq kw :eq)
                                               (eq kw :mod)
                                               (eq kw :range)
                                               (eq kw :and)
                                               (eq kw :or)
                                               (eq kw :neq)))))
                          #'t:unique)
                  #'t:cons))

;; TODO: 2025-06-08 Start here. Note that it'll probably be backwards from what
;; I was originally imagining: the user passes a number which is then fed across
;; predicate functions to determine which plural category it belongs to. So it's
;; (number -> keyword), not the other way around. Then, based on that category
;; keyword, I can select a translation line at the Fluent level.

(defmacro rule->lisp (rule)
  "Expand a rule 'AST' into a Lisp function call.")

(defmacro rules->lisp (rules-ht)
  "Expand a collection of rules into a function that yields a plural category
depending on the results of some predicates.")

#+nil
(->> (uiop:read-file-string #p"data/plurals.xml")
     (rules-by-locale)
     (gethash :af))

;; (defmacro foo (x)
;;   `(+ 1 ,x))

;; (macroexpand-1 '(foo 5))
