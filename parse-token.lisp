;;;; Token parsing infrastructure
;;;

(in-package :org.tfeb.toys.reeder)

(defstruct token-parser
  name
  priority
  denatured
  function)

(defmacro define-token-parser ((name priority &key (denatured 'nil)
                                     (reedtable '*reedtable*))
                               (pattern &rest registers)
                                    &body forms)
  (let ((tokn (make-symbol "tok")))
    `(progn
       (let* ((parser
               (make-token-parser
                :name ',name
                :priority ,priority
                :denatured ,denatured
                :function (lambda (,tokn)
                            (block ,name
                              (register-groups-bind ,registers
                                  ((load-time-value (create-scanner ',pattern))
                                   ,tokn)
                                (return-from ,name
                                  (values (progn ,@forms) t)))
                              (values nil nil)))))
              (table ,reedtable)
              (found (assoc ',name (reedtable-token-parsers table))))
         (if found
             (setf (cdr found) parser)
           (push (cons ',name parser) (reedtable-token-parsers table)))
         (setf (reedtable-token-parsers table)
               (sort (reedtable-token-parsers table)
                     #'> :key (lambda (e)
                                (token-parser-priority (cdr e))))))
       ',name)))

(defun remove-token-parser (name &optional (reedtable *reedtable*))
  (setf (reedtable-token-parsers reedtable)
        (remove name (reedtable-token-parsers reedtable) :key #'first))
  name)

(defun parse-token (tok &key (denatured nil) (reedtable *reedtable*))
  ;; Parse a token: if denatured parse it as a denatured token (use
  ;; the first denatured parser).  Return parse and t or nil and nil.
  (dolist (p (reedtable-token-parsers reedtable))
    (let ((parser (cdr p)))
      (when (or (not denatured)
                (token-parser-denatured parser))
        (multiple-value-bind (value parsedp)
            (funcall (token-parser-function parser) tok)
          (when parsedp
            (return-from parse-token (values value t)))))))
  (values nil nil))
