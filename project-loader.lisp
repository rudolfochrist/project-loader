;;;; project-loader.lisp

(defpackage #:project-loader
  (:nicknames #:pl)
  (:use :cl)
  (:export
   #:create-load-file))

(in-package #:project-loader)


(defun create-load-file (&optional path)
  "Create load.lisp file at PATH.

If PATH is nil use current working directory."
  (uiop:copy-file (asdf:system-relative-pathname "project-loader" "templates/load.lisp")
                  (make-pathname :defaults (or path
                                               (uiop:getcwd))
                                 :name "load"
                                 :type "lisp")))
