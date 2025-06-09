;;; Mostly following this grammar:
;;;
;;; ```
;;; condition       = and_condition ('or' and_condition)*
;;; and_condition   = relation ('and' relation)*
;;; relation        = is_relation | in_relation | within_relation
;;; is_relation     = expr 'is' ('not')? value
;;; in_relation     = expr (('not')? 'in' | '=' | '!=') range_list
;;; within_relation = expr ('not')? 'within' range_list
;;; expr            = operand (('mod' | '%') value)?
;;; operand         = 'n' | 'i' | 'f' | 't' | 'v' | 'w' | 'c' | 'e'
;;; range_list      = (range | value) (',' range_list)*
;;; range           = value'..'value
;;; value           = digit+
;;; digit           = [0-9]
;;; digitPos        = [1-9]
;;; ```
;;;
;;; Except where parts regarding the different relation types were found to not
;;; actually be used in modern data.

(in-package :plurals/gen)

(defparameter +skip-space+ (p:consume (lambda (c) (or (equal c #\space) (equal c #\tab)))))

(defun p-rule (s)
  "There might not be any rule present."
  (p:parse (<* (p:opt #'p-condition) +skip-space+ #'p-example) s))

#+nil
(p-rule "n = 1 @integer")
#+nil
(p-rule "@integer")

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
