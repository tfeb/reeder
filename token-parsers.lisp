;;;; Token parsers
;;;
;;; Parsers here need to be be defined into the precursor reedtable
;;;

(in-package :org.tfeb.toys.reeder)

;;; Here are the things we can hack
;;;

(defun decimal-string->natural (s)
  ;; PARSE-INTEGER would do this, but I wanted to reinvent wheels
  (do* ((zero (char-code #\0))
        (imax (1- (length s)))
        (i 0 (1+ i))
        (v (* (- (char-code (char s i)) zero)
              (expt 10 (- imax i)))
           (+ v (* (- (char-code (char s i)) zero)
                   (expt 10 (- imax i))))))
       ((= i imax) v)))

(define-token-parser (integer 1 :reedtable *precursor-reedtable*)
    ;; General integers, which are always base 10
    ((:sequence
      :start-anchor
      (:register (:greedy-repetition 0 1 (:char-class #\+ #\-)))
      (:register (:greedy-repetition 1 nil (:char-class (:range #\0 #\9))))
      :end-anchor)
     sign digits)
  (* (if (member sign '("" "+") :test #'string=) 1 -1)
     (decimal-string->natural digits)))

(define-token-parser (rational 1 :reedtable *precursor-reedtable*)
    ;; Rationals, also base 10
    ((:sequence
      :start-anchor
      (:register (:greedy-repetition 0 1 (:char-class #\+ #\-)))
      (:register (:greedy-repetition 1 nil (:char-class (:range #\0 #\9))))
      #\/
      (:register (:greedy-repetition 1 nil (:char-class (:range #\0 #\9))))
      :end-anchor)
     sign numerator denominator)
  (* (if (member sign '("" "+") :test #'string=) 1 -1)
     (/ (decimal-string->natural numerator)
        (decimal-string->natural denominator))))

;;; The zero-prioriry parser parses tokens as symbols which are just
;;; interned into *PACKAGE* (there are no uninterned symbols)
;;;

(define-token-parser (symbol 0 :denatured t :reedtable *precursor-reedtable*)
    ((:sequence
      :start-anchor
      (:register (:greedy-repetition 0 nil :everything))
      :end-anchor)
     name)
  (values (intern (string-upcase name) *package*)))
