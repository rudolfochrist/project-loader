;;;; project-loader.lisp

(defpackage #:project-loader
  (:nicknames #:pl)
  (:use :cl)
  (:export
   #:create-load-file
   #:local-system-searcher
   #:register-local-system-searcher
   #:load-system
   #:bundle-dependencies))

(in-package #:project-loader)


(defun create-load-file (&optional path)
  "Create load.lisp file at PATH.

If PATH is nil use current working directory."
  (uiop:copy-file (asdf:system-relative-pathname "project-loader" "templates/load.lisp")
                  (make-pathname :defaults (or path
                                               (uiop:getcwd))
                                 :name "load"
                                 :type "lisp")))

(defun load-system (system-name &optional (path *default-pathname-defaults*))
  (let ((*default-pathname-defaults* (pathname path)))
    #+quicklisp (ql:quickload system-name)
    #-quicklisp (asdf:load-system system-name)))


(defun register-local-system-searcher ()
  (pushnew #'local-system-searcher asdf:*system-definition-search-functions*))


(defun local-system-searcher (system-name)
  (let ((primary-system-name (asdf:primary-system-name system-name)))
    (probe-file (make-pathname :defaults *default-pathname-defaults*
                               :name primary-system-name
                               :type "asd"))))

(defun system-deps (system)
  (remove-if (complement #'stringp)
             (asdf:system-depends-on (asdf:find-system system))))

(defun bundle-dependencies(system &optional (dir "lib/"))
  #-quicklisp (cerror "Please install quicklisp to use the bundle feature.")
  #+quicklisp (ql:bundle-systems (system-deps system) :to dir))
