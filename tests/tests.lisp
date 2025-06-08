(defpackage cldr-plurals/tests
  (:use :cl :parachute)
  (:local-nicknames (:cldr :cldr-plurals)))

(in-package :cldr-plurals/tests)

(define-test suite)

(define-test operators
  ;; 1
  (is = 1 (cldr:op-n "1"))
  (is = 1 (cldr:op-i "1"))
  (is = 0 (cldr:op-v "1"))
  (is = 0 (cldr:op-f "1"))
  (is = 0 (cldr:op-t "1"))
  (is = 0 (cldr:op-e "1"))
  ;; 1.0
  (is = 1 (cldr:op-n "1.0"))
  (is = 1 (cldr:op-i "1.0"))
  (is = 1 (cldr:op-v "1.0"))
  (is = 0 (cldr:op-f "1.0"))
  (is = 0 (cldr:op-t "1.0"))
  (is = 0 (cldr:op-e "1.0"))
  ;; 1.00
  (is = 1 (cldr:op-n "1.00"))
  (is = 1 (cldr:op-i "1.00"))
  (is = 2 (cldr:op-v "1.00"))
  (is = 0 (cldr:op-f "1.00"))
  (is = 0 (cldr:op-t "1.00"))
  (is = 0 (cldr:op-e "1.00"))
  ;; 1.3
  (is = 1.3 (cldr:op-n "1.3"))
  (is = 1 (cldr:op-i "1.3"))
  (is = 1 (cldr:op-v "1.3"))
  (is = 3 (cldr:op-f "1.3"))
  (is = 3 (cldr:op-t "1.3"))
  (is = 0 (cldr:op-e "1.3"))
  ;; 1.30
  (is = 1.3 (cldr:op-n "1.30"))
  (is = 1 (cldr:op-i "1.30"))
  (is = 2 (cldr:op-v "1.30"))
  (is = 30 (cldr:op-f "1.30"))
  (is = 3 (cldr:op-t "1.30"))
  (is = 0 (cldr:op-e "1.30"))
  ;; 1.03
  (is = 1.03 (cldr:op-n "1.03"))
  (is = 1 (cldr:op-i "1.03"))
  (is = 2 (cldr:op-v "1.03"))
  (is = 3 (cldr:op-f "1.03"))
  (is = 3 (cldr:op-t "1.03"))
  (is = 0 (cldr:op-e "1.03"))
  ;; 1.230
  (is = 1.23 (cldr:op-n "1.230"))
  (is = 1 (cldr:op-i "1.230"))
  (is = 3 (cldr:op-v "1.230"))
  (is = 230 (cldr:op-f "1.230"))
  (is = 23 (cldr:op-t "1.230"))
  (is = 0 (cldr:op-e "1.230"))
  ;; 1200000
  (is = 1200000 (cldr:op-n "1200000"))
  (is = 1200000 (cldr:op-i "1200000"))
  (is = 0 (cldr:op-v "1200000"))
  (is = 0 (cldr:op-f "1200000"))
  (is = 0 (cldr:op-t "1200000"))
  (is = 0 (cldr:op-e "1200000"))
  ;; 1200.50
  (is = 1200.50 (cldr:op-n "1200.50"))
  (is = 1200 (cldr:op-i "1200.50"))
  (is = 2 (cldr:op-v "1200.50"))
  (is = 50 (cldr:op-f "1200.50"))
  (is = 5 (cldr:op-t "1200.50"))
  (is = 0 (cldr:op-e "1200.5")))
