;;;; A quote (') reed macro
;;;

(org.tfeb.tools.require-module:needs
 (:org.tfeb.toys.reeder :compile t))

(defpackage :org.tfeb.toys.reeder.quote-reeder
  (:use
   :cl
   :org.tfeb.toys.reeder))

(in-package :org.tfeb.toys.reeder.quote-reeder)

(defvar *qr-reedtable* (copy-reedtable nil))

(setf (reedtable-macro-character #\' *qr-reedtable*)
      (lambda (from quote table)
        (declare (ignore quote))
        (values `(quote ,(reed :from from :reedtable table))
                (inch from nil))))
