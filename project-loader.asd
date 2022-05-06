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
  :depends-on ((:require "uiop"))
  :components ((:static-file "templates/load.lisp")
               (:file "project-loader"))
  :description "A utility to load Quicklisp systems without loading Quicklisp itself."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :perform (load-op (op c)
                    (uiop:symbol-call :project-loader :register-local-system-searcher)))
