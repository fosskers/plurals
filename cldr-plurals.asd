;; TODO: Change name to plurals

(defsystem "cldr-plurals"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/cldr-plurals"
  :depends-on ()
  :serial t
  :components ((:module "src" :components ((:file "package"))))
  :description ""
  :in-order-to ((test-op (test-op :cldr-plurals/tests))))

(defsystem "cldr-plurals/gen"
  :depends-on (:parcom :parcom/xml :transducers :arrow-macros :cldr-plurals)
  :components ((:module "src" :components ((:file "gen")
                                           (:file "parsers"))))
  :description "Generate CL source code based on plural rules in XML.")

(defsystem "cldr-plurals/tests"
  :depends-on (:cldr-plurals :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :cldr-plurals/tests)))
