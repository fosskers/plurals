(defpackage plurals/gen
  (:use :cl :arrow-macros)
  (:import-from :parcom #:<*> #:<* #:*> #:<$)
  (:local-nicknames (#:x #:parcom/xml)
                    (#:t #:transducers)
                    (#:p #:parcom)))

(in-package :plurals/gen)

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

;; --- Generation of Common Lisp --- ;;

(deftype operator ()
  '(member :n :i :v :f :t :e))

(defmacro rules->lisp (path)
  "Expand a collection of rules into a function that yields a plural category
depending on the results of some predicates."
  (labels ((rule->lisp (rule)
             (etypecase rule
               (operator (case rule
                           (:n `n)
                           (:i `i)
                           (:v `v)
                           (:f `f)
                           (:t `tee)
                           (:e `e)))
               (list (destructuring-bind (f var &rest vals) rule
                       (let ((v (rule->lisp var)))
                         (case f
                           (:eq  (cond ((numberp (car vals)) `(= ,v ,(car vals)))
                                       ((eq :range (car (car vals)))
                                        `(<= ,(nth 1 (car vals)) ,v ,(nth 2 (car vals))))
                                       ((listp (car vals))
                                        `(let ((x ,v))
                                           (or ,@(mapcar (lambda (n) (rule->lisp (list :eq 'x n))) (car vals)))))))
                           (:neq `(not ,(rule->lisp (cons :eq (cons var vals)))))
                           (:mod `(mod ,v ,(car vals)))
                           (:and `(and ,@(mapcar #'rule->lisp (cons var vals))))
                           (:or  `(or ,@(mapcar #'rule->lisp (cons var vals))))))))
               (symbol rule))))
    `(defun category (locale s)
       "Given a string of a number and a target locale, determine the plural category of the number."
       (let ((n (plurals::op-n s))
             (i (plurals::op-i s))
             (v (plurals::op-v s))
             (f (plurals::op-f s))
             (tee (plurals::op-t s))
             (e (plurals::op-e s)))
         (case locale
           ,@(t:transduce
              (t:map (lambda (pair)
                       (cond ((zerop (hash-table-count (cdr pair)))
                              `(,(car pair) :other))
                             (t `(,(car pair) (cond ,@(t:transduce (t:map (lambda (rule) `(,(rule->lisp (cdr rule)) ,(car rule))))
                                                                   #'t:cons (cdr pair))
                                                    (t :other)))))))
              #'t:cons
              (rules-by-locale (uiop:read-file-string path))))))))

#+nil
(rules->lisp #p"data/plurals.xml")

#+nil
(->> (uiop:read-file-string #p"data/plurals.xml")
     (rules-by-locale)
     (gethash :kw))
