;;;; Low-level support
;;;

(in-package :org.tfeb.toys.reeder)

(define-condition reeder-error (parse-error)
  ;; all reeder errors are these
  ())

(define-condition simple-reeder-error (reeder-error simple-error)
  ;; simple reeder errors don't know about streams
  ())

(define-condition streamy-reeder-error (reeder-error simple-error stream-error)
  ;; streamy ones do
  ())

(defun simple-reeder-error (control &rest format-args)
  (error 'simple-reeder-error
         :format-control control
         :format-arguments format-args))

(defun streamy-reeder-error (stream control &rest format-args)
  (error 'streamy-reeder-error
         :stream stream
         :format-control control
         :format-arguments format-args))

(defun inch (from &optional (eof-value nil eof-value-p))
  (let ((c (read-char from nil nil)))
    (if (null c)
        (if eof-value-p
            eof-value
          (streamy-reeder-error from "EOF"))
      c)))