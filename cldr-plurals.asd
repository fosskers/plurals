(defsystem "cldr-plurals"
  :version "0.0.0"
  :author ""
  :license ""
  :homepage ""
  :depends-on ()
  :serial t
  :components ((:module "src" :components ((:file "package"))))
  :description "")

(defsystem "cldr-plurals/gen"
  :depends-on (:parcom :parcom/xml :transducers :arrow-macros)
  :components ((:module "src" :components ((:file "gen")
                                           (:file "parsers")))))
