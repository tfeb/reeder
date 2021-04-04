;;;; Interface
;;;

(in-package :org.tfeb.toys.reeder)

(defun next-interesting-character (from first table)
  ;; return the next interesting character, or NIL if EOF
  (do ((c first (inch from nil)))
      ((or (null c)
           (not (char-whitespace-p c table))
           (reedtable-macro-character c table))
       c)))

(defun reed-one-thing (from first table)
  (let ((c (next-interesting-character from first table)))
    (when (null c)                      ;hit EOF
      (streamy-reeder-error from "EOF when reading"))
    (let ((reedmacro (reedtable-macro-character c table)))
      (funcall (or reedmacro
                   (reedtable-token-reader table))
               from c table))))

(defun reed (&key (from *standard-input*)
                  (reedtable *reedtable*))
  (multiple-value-bind (thing next) (reed-one-thing from
                                                    (inch from)
                                                    reedtable)
    ;; I am not sure about this
    (when (and next (not (char-whitespace-p next reedtable)))
      (unread-char next from))
    thing))
