;;;; load.lisp

;;;
;;; This file is generate by `project-loader'.
;;;

(require 'asdf)
#+sbcl (require 'sb-aclrepl)

(defpackage #:loader
  (:use :cl)
  (:export
   #:current-directory-search
   #:*quicklisp-location*
   #:*bundle-location*))

(in-package #:loader)

;;; Configuration

(defparameter *quicklisp-location* "~/quicklisp/"
  "Quicklisp installation path.")

(defparameter *bundle-location* "vendor/"
  "Project-local path to bundled systems.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                         ;;;
;;; !!! WARNING !!!                                                         ;;;
;;;                                                                         ;;;
;;; Don't touch anything below unless you know what you're doing.           ;;;
;;;                                                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun current-directory-search (name)
  "Search the current directory for system NAME."
  (probe-file (make-pathname :defaults (uiop:getcwd)
                             :name (asdf:primary-system-name name)
                             :type "asd")))

;;; TODO: Take dist preference into consideration!

;;; register search functions
(push #'current-directory-search asdf:*system-definition-search-functions*)

(let ((bundle-file (merge-pathnames
                    "bundle.lisp"
                    (merge-pathnames *bundle-location* (uiop:getcwd)))))
  (when (probe-file bundle-file)
    (load bundle-file)))
