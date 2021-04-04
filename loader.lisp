;;;; Enough for REQUIRE-MODULE to be able to load reeder
;;;

(in-package :cl-user)

#+LispWorks
(progn
  (load (make-pathname :name "sysdcl" :defaults *load-pathname*))
  (compile-system ':org.tfeb.toys.reeder :load t))

#-LispWorks
(require "ASDF")

#-LispWorks
(progn
  (load (make-pathname :name "org.tfeb.toys.reeder" :type "asd"
                       :defaults *load-pathname*))
  (asdf:load-system "org.tfeb.toys.reeder"))
