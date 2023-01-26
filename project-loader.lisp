;;;; project-loader.lisp

(defpackage #:project-loader
  (:nicknames #:pl)
  (:use :cl)
  (:export
   #:create-load-file
   #:local-system-searcher
   #:register-local-system-searcher
   #:bundle-dependencies))

(in-package #:project-loader)


(defun create-load-file (&optional path)
  "Create load.lisp file at PATH.

If PATH is nil use current working directory."
  (let ((load-lisp (make-pathname :defaults (or path
                                                (uiop:getcwd))
                                  :name "load"
                                  :type "lisp")))
    (uiop:copy-file (asdf:system-relative-pathname "project-loader" "templates/load.lisp")
                    load-lisp)
    load-lisp))


(defun register-local-system-searcher ()
  (pushnew #'local-system-searcher asdf:*system-definition-search-functions*)
  (format t "~&; Registered local-system-searcher~%")
  (force-output))


(defun local-system-searcher (system-name)
  (let ((primary-system-name (asdf:primary-system-name system-name)))
    (probe-file (make-pathname :defaults *default-pathname-defaults*
                               :name primary-system-name
                               :type "asd"))))

(defun system-deps (system)
  (remove-if (complement #'stringp)
             (asdf:system-depends-on (asdf:find-system system))))

(defun resolve-dependencies (system)
  (loop with q = (system-deps system)
        while q
        for sys = (pop q)
        if (ql-dist:find-system sys)
          collect sys into ql-deps
        else if (asdf:find-system sys)
               collect (asdf:system-source-directory sys) into paths
               and do (setf q (system-deps sys))
        finally (return (list ql-deps
                              (remove-duplicates paths :test #'equal)))))

(defun copy-directory (from to)
  (let* ((dest-dir (pathname-directory (merge-pathnames to (uiop:getcwd))))
         (from-dir (pathname-directory from))
         (name (car (last from-dir)))
         (name-pos (position name from-dir :test #'string=)))
    (dolist (file (uiop:directory-files from uiop/pathname:*wild-path*))
      (uiop:copy-file file
                      (ensure-directories-exist
                       (make-pathname
                        :defaults file
                        :directory (append dest-dir
                                           (nthcdr name-pos (pathname-directory file)))))))))


(defun bundle-dependencies(system &optional (dir "lib/"))
  #-quicklisp (cerror "Please install quicklisp to use the bundle feature.")
  #+quicklisp
  (destructuring-bind (ql-deps local-deps)
      (resolve-dependencies system)
    (format t "~&; Bundling quicklisp.~%")
    (ql:bundle-systems ql-deps :to dir)
    (when local-deps
      (dolist (dep local-deps)
        (format t "~&; Copy local dependency: ~A~%" dep)
        (copy-directory
         dep
         (merge-pathnames "local-projects/"
                          (merge-pathnames dir (uiop:getcwd))))))
    t))
