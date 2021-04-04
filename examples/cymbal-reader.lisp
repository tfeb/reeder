;;;; Make reeder read 'cymbals' instead of symbols
;;;

(org.tfeb.tools.require-module:needs
 (:org.tfeb.toys.reeder :compile t))

(defpackage :org.tfeb.play
  (:use                                 ;
   :cl
   :org.tfeb.toys.reeder))

(in-package :org.tfeb.play)

(defvar *cymbal-reedtable* (copy-reedtable nil))

(defvar *namespace* (make-hash-table :test #'equal))

(defstruct cymbal
  name)

(defgeneric ensure-cymbal (thing))

(defmethod ensure-cymbal ((thing string))
  (or (gethash thing *namespace*)
      (setf (gethash thing *namespace*)
            (make-cymbal :name thing))))

(defmethod ensure-cymbal ((thing cymbal))
  thing)

(define-token-parser (cymbal 0 :denatured t :reedtable *cymbal-reedtable*)
    ((:sequence
      :start-anchor
      (:register (:greedy-repetition 0 nil :everything))
      :end-anchor)
     name)
  (ensure-cymbal name))
