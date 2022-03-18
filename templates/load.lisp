;;;; load.lisp

;;;
;;; This file is generate by `project-loader'.
;;;

(require 'asdf)
#+sbcl (require 'sb-aclrepl)

(defpackage #:loader
  (:use :cl)
  (:export
   #:*quicklisp-location*
   #:current-directory-search
   #:ql-search
   #:ql-local-search))

(in-package #:loader)

;;; Configuration

(defparameter *quicklisp-location* "~/quicklisp/"
  "Quicklisp installation path.")


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

(defun ql-search (name)
  "Search Quicklisp system for NAME."
  (let ((system (make-pathname :defaults (merge-pathnames
                                          "dists/quicklisp/installed/systems/"
                                          *quicklisp-location*)
                               :name (asdf:primary-system-name name)
                               :type "txt")))
    (when (probe-file system)
      (with-open-file (stream system )
        (let* ((release (read-line stream nil)))
          (probe-file (merge-pathnames release *quicklisp-location*)))))))


(defun ql-local-search (name)
  "Search local Quicklisp system for NAME."
  (let ((primary-name (asdf:primary-system-name name))
        (asds (uiop:directory-files (merge-pathnames
                                     "local-projects/"
                                     *quicklisp-location*)
                                    "**/*.asd")))
    (find-if (lambda (asd)
               (string= primary-name
                        (pathname-name asd)))
             asds)))

;;; register search functions
(setf asdf:*system-definition-search-functions*
      (append (list #'current-directory-search
                    #'ql-search
                    #'ql-local-search)
              asdf:*system-definition-search-functions*))
