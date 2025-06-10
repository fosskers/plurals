(defsystem "plurals"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/plurals"
  :depends-on ()
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "generated"))))
  :description "Plural categories for all Unicode-registered languages."
  :in-order-to ((test-op (test-op :plurals/tests))))

(defsystem "plurals/gen"
  :depends-on (:parcom :parcom/xml :transducers :arrow-macros :plurals)
  :components ((:module "src" :components ((:file "gen")
                                           (:file "parsers"))))
  :description "Generate CL source code based on plural rules in XML.")

(defsystem "plurals/tests"
  :depends-on (:plurals :parachute)
  :components ((:module "tests" :components ((:file "tests"))))
  :perform (test-op (op c) (symbol-call :parachute :test :plurals/tests)))
