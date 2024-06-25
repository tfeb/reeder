;;;; Read decimal numbers as rationals
;;;

(org.tfeb.tools.require-module:needs
 ((:org.tfeb.hax.collecting
   :org.tfeb.toys.reeder)
  :compile t))

(defpackage :org.tfeb.play
  (:use
   :cl
   :org.tfeb.toys.reeder
   :org.tfeb.hax.collecting))

(in-package :org.tfeb.play)

(defvar *rational-decimal-reedtable* (copy-reedtable))

(define-token-parser (rational-decimal 2 :reedtable *rational-decimal-reedtable*)
    ((:sequence
      :start-anchor
      ;; sign then digits
      (:greedy-repetition 0 1 (:register (:char-class #\+ #\-)))
      (:register (:greedy-repetition 0 nil (:char-class (:range #\0 #\9))))
      ;; A decimal point
      #\.
      ;; maybe an exponent
      (:greedy-repetition
       0
       1
       (:group
        (:sequence
         (:register (:greedy-repetition 0 nil (:char-class (:range #\0 #\9))))
         (:greedy-repetition
          0
          1
          (:group
           (:sequence
            (:char-class #\e #\E)
            (:greedy-repetition 0 1 (:register (:char-class #\+ #\-)))
            (:register
             (:greedy-repetition 0 nil (:char-class (:range #\0 #\9))))))))))
      :end-anchor)
     sign digits decimal-digits exponent-sign exponent-digits)
  ;; Yet another cheapo PARSE-INTEGER
  (flet ((->natural (s)
           (etypecase s
             (null 0)
             (string
              (with-accumulators ((r (lambda (c n)
                                       (+ (* c 10) n))
                                     0))
                (dotimes (i (length s))
                  (let ((d (digit-char-p (char s i) 10)))
                    (unless d (error "bad digit string ~S" s))
                    (r d)))))))
         (->sign (s)
           (etypecase s
             (null 1)
             (string
              (cond
               ((or (string= s "")
                    (string= s "+"))
                1)
               ((string= s "-")
                -1)
               (t
                (error "bad sign ~S" s)))))))
    (* (->sign sign)
       (+ (->natural digits)
          (let ((decimal (->natural decimal-digits)))
            (/ decimal (expt 10 (ceiling (log decimal 10))))))
       (expt 10
             (* (->sign exponent-sign)
                (->natural exponent-digits))))))
