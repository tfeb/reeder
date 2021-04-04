;;;; Reeders (except the cons reader)
;;;
;;; Everything is called with stream, char, reedtable
;;; Reeders should return the thing read and the next character, or nil

(in-package :org.tfeb.toys.reeder)

(defun reed-token (from first table &key (parsed t))
  ;; PARSED is for testing: the reedtable never uses it
  (let ((sesc (reedtable-single-escape table))
        (mesc (reedtable-multiple-escape table)))
    (iterate next ((c first)
                   (sescp nil)
                   (mescp nil)
                   (denatured nil)
                   (stluser '()))
      (cond
       ((null c)
        ;; EOF is OK: check it early so we don't have to worry about c
        ;; not being a character
        (let ((raw  (coerce (reverse stluser) 'string)))
          (values (if parsed
                      (multiple-value-bind (parse handled)
                          (parse-token raw :denatured denatured :reedtable table)
                        (unless handled
                          (streamy-reeder-error from "unparsed token ~S" raw))
                        parse)
                    raw)
                nil)))
       (sescp
        (next (inch from nil)
              nil nil t
              (cons c stluser)))
       (mescp
        (if (char= c mesc)
            (next (inch from nil)
                  nil nil t
                  stluser)
          (next (inch from)
                nil t t
                (cons c stluser))))
       ((and sesc (char= c sesc))
        (next (inch from nil)
              t nil t
              stluser))
       ((and mesc (char= c mesc))
        (next (inch from nil)
              nil t t
              stluser))
       ((or (char-whitespace-p c table)
            (reedtable-macro-character c table))
        (let ((raw (coerce (reverse stluser) 'string)))
          (values
           (if parsed
               (multiple-value-bind (parse handled)
                   (parse-token raw :denatured denatured :reedtable table)
                 (unless handled
                   (streamy-reeder-error from "unparsed token ~S" raw))
                 parse)
             raw)
           c)))
       (t
        (next (inch from nil)
              nil nil denatured
              (cons c stluser)))))))

(setf (reedtable-token-reader *precursor-reedtable*)
      'reed-token)

(defun reed-delimited-string (from delimiter table)
  ;; read a delimited string: return the string and the next
  ;; character.  Probably this should be split into this function and
  ;; the reeder function which calls it, but never mind.
  (let ((sesc (reedtable-single-escape table))
        (mesc (reedtable-multiple-escape table)))
    (iterate next ((c (inch from))
                   (sescp nil)
                   (mescp nil)
                   (stluser '()))
      (cond
       (sescp
        (next (inch from)
              nil nil
              (cons c stluser)))
       (mescp
        (if (char= c mesc)
            (next (inch from)
                  nil nil
                  stluser)
          (next (inch from)
                nil t
                stluser)))
       ((char= c sesc)
        (next (inch from)
              t nil
              stluser))
       ((char= c mesc)
        (next (inch from)
              nil t
              stluser))
       ((char= c delimiter)
        (values (coerce (reverse stluser) 'string)
                (inch from nil)))
       (t
        (next (inch from)
              nil nil
              (cons c stluser)))))))

(setf (reedtable-macro-character #\" *precursor-reedtable*)
      'reed-delimited-string)
