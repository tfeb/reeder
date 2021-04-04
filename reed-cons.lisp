;;;; The cons reader
;;;
;;; This is late on because it needs to invoke the reeder recursively
;;; It's also misnmed: the cons reader can return () of course.
;;;

(in-package :org.tfeb.toys.reeder)

;;; A special token parser for consing dots
;;;

(defconstant consing-dot (copy-seq "."))

(defvar *consing-dot-ok-p* nil)         ;normally not OK

(define-token-parser (consing-dot 1 :reedtable *precursor-reedtable*)
    ;; Consing dot
    ((:sequence
      :start-anchor
      #\.
      :end-anchor))
  (if *consing-dot-ok-p*
      consing-dot
    (simple-reeder-error "unexpected consing dot")))

(defun reed-illegal-macro-character (from char table)
  ;; This will live on #\) for instance
  (declare (ignore table))
  (streamy-reeder-error from "illegal macro character ~S" char))

(defconstant parenlike-pairs
  ;; just parens for now
  '((#\( . #\))))

(defun reed-cons (from lparen table)
  (let ((rparen (cdr (assoc lparen parenlike-pairs))))
    (unless rparen
      (streamy-reeder-error from "bad left parenlike character ~S" lparen))
  (values (reed-delimited-cons from rparen table)
          (inch from nil))))

(defun reed-delimited-cons (from rparen table)
  ;; Read a cons (or '()) whose right paren is rparen
  (let ((c (next-interesting-character from (inch from) table)))
    (cond
     ((null c)
      (streamy-reeder-error from "EOF"))
     ((char= c rparen)
      '())
     (t
      (multiple-value-bind (thing next)
          (let ((*consing-dot-ok-p* nil))
            (reed-one-thing from c table))
        (cons thing (reed-delimited-cons-tail from next rparen table)))))))

(defun reed-delimited-cons-tail (from this rparen table)
  ;; Read the tail of a cons
  (let ((c (next-interesting-character from this table)))
    (cond
     ((null c)
      (streamy-reeder-error from "EOF"))
     ((char= c rparen)
      '())
     (t
      (multiple-value-bind (thing next)
          (let ((*consing-dot-ok-p* t))
            (reed-one-thing from c table))
        (if (eq thing consing-dot)
            (reed-delimited-cdr from next rparen table)
          (cons thing (reed-delimited-cons-tail from next rparen table))))))))

(defun reed-delimited-cdr (from this rparen table)
  (let ((c (next-interesting-character from this table)))
    (if (null c)
        (streamy-reeder-error from "EOF")
      (multiple-value-bind (thing next) (reed-one-thing from c table)
        (let ((close (next-interesting-character from next table)))
          (cond
           ((null close)
            (streamy-reeder-error from "EOF"))
           ((not (char= close rparen))
            (streamy-reeder-error from "botched dotted pair"))
           (t thing)))))))

(dolist (parenlike parenlike-pairs)
  (setf (reedtable-macro-character (car parenlike) *precursor-reedtable*)
        'reed-cons
        (reedtable-macro-character (cdr parenlike) *precursor-reedtable*)
        'reed-illegal-macro-character))
