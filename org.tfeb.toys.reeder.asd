;;;; ASDF sysdcl for reeder
;;;

(in-package :asdf-user)

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.iterate :compile t))

(defsystem "org.tfeb.toys.reeder"
  :description "A toy reader"
  :version "0.0.0"
  :author "Tim Bradshaw"
  :license "MIT"
  :depends-on (#-org.tfeb.tools.require-module
               "org.tfeb.hax"
               "cl-ppcre")
  :serial t
  :components
  ((:file "pkg")
   (:file "low")
   (:file "reedtables")
   (:file "parse-token")
   (:file "token-parsers")
   (:file "reeders")
   (:file "reed")
   (:file "reed-cons")
   (:file  "cometh")))
