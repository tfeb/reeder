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

;;; The symbol parser parses tokens as symbols and now tries to deal
;;; with packages (this is rather untested)
;;;

(define-token-parser (symbol 1 :denatured t :reedtable *precursor-reedtable*)
    ((:sequence
     :start-anchor
     ;; packege prefix perhaps
     (:greedy-repetition
      0
      1
      (:sequence
       (:register (:greedy-repetition 0 nil (:inverted-char-class #\:)))
       (:register (:greedy-repetition 1 2 (:char-class #\:)))))
     ;; Symbol name
     (:register (:greedy-repetition 0 nil (:inverted-char-class #\:)))
     :end-anchor)
     package-name colon/s name)
  (let ((package (if colon/s
                     (find-package
                      (if (zerop (length package-name))
                          "KEYWORD"
                        (string-upcase package-name)))
                   *package*)))
    (unless package
      (simple-reeder-error "no package ~A" (string-upcase package-name)))
    (if colon/s
        (if (or (= (length colon/s) 2)
                (eq package (find-package "KEYWORD")))
            ;; Easy, just intern it
            (values (intern (string-upcase name) package))
          (multiple-value-bind (s status) (find-symbol (string-upcase name) package)
            (unless (eq status :external)
              (simple-reeder-error "~A is not external in ~A" s (package-name package)))
            s))
      ;; No colons
      (values (intern (string-upcase name) package)))))

;;; Fallback parser now just signals an error
;;;

(define-token-parser (fallback 0 :denatured t :reedtable *precursor-reedtable*)
    ((:sequence
      :start-anchor
      (:register (:greedy-repetition 0 nil :everything))
      :end-anchor)
     thing)
  (simple-reeder-error "~S isn't anything I recognise: probably a botched symbol?" thing))
