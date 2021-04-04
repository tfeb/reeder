;;;; Bootstrap
;;;

(in-package :org.tfeb.toys.reeder)

;;; Initialise the default reedtable from the precursor table
;;;
(setf *reedtable* (copy-reedtable nil))

;;; Here we are now, entertain us
;;;
(provide :org.tfeb.toys.reeder)
