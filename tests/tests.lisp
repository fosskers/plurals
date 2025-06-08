(defpackage cldr-plurals/tests
  (:use :cl :parachute)
  (:local-nicknames (:cldr :cldr-plurals)))

(in-package :cldr-plurals/tests)

(define-test suite)

(defmacro operator (s n i v f tee e)
  `(progn
     (is = ,n (cldr:op-n ,s))
     (is = ,i (cldr:op-i ,s))
     (is = ,v (cldr:op-v ,s))
     (is = ,f (cldr:op-f ,s))
     (is = ,tee (cldr:op-t ,s))
     (is = ,e (cldr:op-e ,s))))

(define-test operators
  (operator "1" 1 1 0 0 0 0)
  (operator "1.0" 1 1 1 0 0 0)
  (operator "1.00" 1 1 2 0 0 0)
  (operator "1.3" 1.3 1 1 3 3 0)
  (operator "1.30" 1.3 1 2 30 3 0)
  (operator "1.03" 1.03 1 2 3 3 0)
  (operator "1.230" 1.23 1 3 230 23 0)
  (operator "1200000" 1200000 1200000 0 0 0 0)
  (operator "1200.50" 1200.50 1200 2 50 5 0))
