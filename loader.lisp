;;;; Enough for REQUIRE-MODULE to be able to load reeder
;;;

(in-package :cl-user)

(require "ASDF")

(load (make-pathname :name "org.tfeb.toys.reeder" :type "asd"
                     :defaults *load-pathname*))
(asdf:load-system "org.tfeb.toys.reeder")
