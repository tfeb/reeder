;;;; LW sysdcl for reeder
;;;

(in-package :cl-user)

#-(or LispWorks)
(eval-when (:load-toplevel :compile-toplevel :execute)
  (error "Not implemented"))

#-(and quicklisp org.tfeb.tools.require-module)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (error "Need quicklisp & require-module"))

(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.iterate :compile t))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "cl-ppcre"))

(defsystem :org.tfeb.toys.reeder
  ;; This is now mindless again: just do everything in series
  ()
  :members
  ("pkg"
   "low"
   "reedtables"
   "parse-token"
   "token-parsers"
   "reeders"
   "reed"
   "reed-cons"
   "cometh"
   ("sysdcl" :root-module nil
             :source-only t))
  :rules
  ((:in-order-to :compile :all
    (:requires (:load :previous)))
   (:in-order-to :load :all
    (:requires (:load :previous)))))
