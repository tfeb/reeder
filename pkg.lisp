;;;; Package definition for reeder
;;;

(in-package :cl-user)

(defpackage :org.tfeb.toys.reeder
  (:use
   :cl
   :cl-ppcre
   :org.tfeb.hax.iterate)
  (:export
   ;; Low: should export some accessors maybe?
   #:reeder-error
   #:inch)
  (:export
   ;; reedtables
   #:reedtable-single-escape
   #:reedtable-multiple-escape
   #:reedtable-whitespace
   #:reedtable-token-parser-names
   #:reedtable-token-reader
   #:reedtable-macro-character
   #:copy-reedtable
   #:*reedtable*)
  (:export
   ;; Token parsing and the standard parsers
   #:define-token-parser
   #:remove-token-parser
   #:parse-token)
  (:export
   #:reed-token                         ;for testing
   #:reed-delimited-string)             ;for testing
  (:export
   #:reed))
