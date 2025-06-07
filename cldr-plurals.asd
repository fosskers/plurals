(defsystem "cldr-plurals"
  :version "0.0.0"
  :author "Colin Woodbury <colin@fosskers.ca>"
  :license "MPL-2.0"
  :homepage "https://github.com/fosskers/cldr-plurals"
  :depends-on ()
  :serial t
  :components ((:module "src" :components ((:file "package"))))
  :description "")

(defsystem "cldr-plurals/gen"
  :depends-on (:parcom :parcom/xml :transducers :arrow-macros)
  :components ((:module "src" :components ((:file "gen")
                                           (:file "parsers"))))
  :description "Generate CL source code based on plural rules in XML.")
