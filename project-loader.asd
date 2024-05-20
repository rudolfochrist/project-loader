;;;; project-loader.asd

(defsystem "project-loader"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/project-loader"
  :bug-tracker "https://github.com/rudolfochrist/project-loader/issues"
  :source-control (:git "https://github.com/rudolfochrist/project-loader.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop")
               "cl-ppcre-unicode")
  :components ((:static-file "init.lisp")
               (:static-file "install-dependecies")
               (:file "project-loader"))
  :description "A utility to load Quicklisp systems without loading Quicklisp itself."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt")))
