;;;; Reedtables
;;;
;;; These are like much simpler readtables: they keep track of macro
;;; characters, escape characters, whitespace characters and contain
;;; the token parsers (so all parsing is driven by the reedtable).
;;; All macro characters are terminating.
;;;
;;; It's OK to destructively modify parts of these: the copier is
;;; responsible for making sure that everything that is mutable is
;;; copied.
;;;

(in-package :org.tfeb.toys.reeder)

(defclass reedtable ()
  ((single-escape :initform #\\
                  :initarg :single-escape
                  :accessor reedtable-single-escape)
  (multiple-escape :initform #\|
                   :initarg :multiple-escape
                   :accessor reedtable-multiple-escape)
  (whitespace :initform '(#\Space #\Tab #\Newline)
              :initarg :whitespace
              :accessor reedtable-whitespace)
  (macro-characters :initform '()
                    :initarg :macro-characters
                    :accessor reedtable-macro-characters)
  (token-reader :initarg :token-reader
                :accessor reedtable-token-reader)
  (token-parsers :initform '()
                 :initarg :token-parsers
                 :accessor reedtable-token-parsers)))

;; This is like the standard readtable, but it's not public
(defvar *precursor-reedtable* (make-instance 'reedtable))

(defgeneric make-reedtable-from-reedtable (from)
  (:method ((from reedtable))
   (make-instance (class-of from)
                  :single-escape (reedtable-single-escape from)
                  :multiple-escape (reedtable-multiple-escape from)
                  :whitespace (copy-list (reedtable-whitespace from))
                  :macro-characters (copy-alist
                                     (reedtable-macro-characters from))
                  :token-reader (reedtable-token-reader from)
                  :token-parsers (copy-alist (reedtable-token-parsers from)))))

(defvar *reedtable* nil)                ;will be the default reedtable

(defun copy-reedtable (&optional (reedtable *reedtable*))
  (make-reedtable-from-reedtable (or reedtable *precursor-reedtable*)))

(defun char-whitespace-p (c &optional (reedtable *reedtable*))
  ;; Is c whitespace in the reedtable?  Should this punt to a GF?
  (member c (reedtable-whitespace reedtable)))

(defgeneric reedtable-macro-character-for-reedtable (c table)
  (:method ((c character) (table reedtable))
   (cdr (assoc c (reedtable-macro-characters table)))))

(defgeneric (setf reedtable-macro-character-for-reedtable) (new c table)
  (:method ((fn function) (c character) (table reedtable))
   (let ((found (assoc c (reedtable-macro-characters table))))
     (if found
         (setf (cdr found) fn)
       (push (cons c fn) (reedtable-macro-characters table))))
     fn)
  (:method ((fn symbol) (c character) (table reedtable))
   ;; This is identical to the previous method, but lets you use
   ;; symbols, which is more robust as redefining the functions is
   ;; safe and FUNCALL is OK with function designators
   (let ((found (assoc c (reedtable-macro-characters table))))
     (if found
         (setf (cdr found) fn)
       (push (cons c fn) (reedtable-macro-characters table))))
     fn)
  (:method ((void null) (c character) (table reedtable))
   (setf (reedtable-macro-characters table)
         (delete c (reedtable-macro-characters table) :key #'car))))

(defun reedtable-macro-character (c &optional (table *reedtable*))
  (reedtable-macro-character-for-reedtable c table))

(defun (setf reedtable-macro-character) (f c &optional (table *reedtable*))
  (setf (reedtable-macro-character-for-reedtable c table) f))

(defun reedtable-token-parser-names (&optional (table *reedtable*))
  (reedtable-token-parser-names-for-reedtable table))

(defgeneric reedtable-token-parser-names-for-reedtable (table)
  (:method ((table reedtable))
   (mapcar #'car (reedtable-token-parsers table))))
