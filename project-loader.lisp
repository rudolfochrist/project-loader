;;;; project-loader.lisp

(defpackage #:project-loader
  (:nicknames #:pl)
  (:use :cl)
  (:export
   #:initialize-registry
   #:load-system
   #:load-systems
   #:pin-dependencies
   #:write-init-file
   #:find-directory-systems
   #:load-directory-systems))

(in-package #:project-loader)

(defun initialize-registry (&optional init-file)
  (load (or init-file (merge-pathnames "init.lisp" (uiop:getcwd)))))

#+quicklisp
(defun load-system (name &key verbose silent)
  (ql:quickload name :verbose verbose :silent silent))

#+(and asdf (not quicklisp))
(defun load-system (name &key verbose)
  (asdf:load-system name :verbose verbose))

(defun load-systems (&rest systems)
  (mapc #'load-system systems))

#+quicklisp
(defun collect-dependency-urls (system-names)
  "Collect the QL release URL for each dependency of SYSTEM"
  (flet ((system-deps (system)
           (ql-dist:required-systems system))
         (archive-url (system)
           (ql-dist:archive-url (ql-dist:release system)))
         (hash-keys (hash-table)
           (loop for k being the hash-keys of hash-table collect k)))
    (loop with deps = (make-hash-table :test 'equal)
          for system-name in system-names
          do (loop with d = (remove-if #'consp (asdf:system-depends-on (asdf:find-system system-name)))
                   while d
                   do (let ((system (ql-dist:find-system (pop d))))
                        (when system
                          (setf d (append d (system-deps system)))
                          (setf (gethash (archive-url system) deps) t))))
          finally (return (hash-keys deps)))))

#+quicklisp
(defun pin-dependencies (system-names &key (outfile #p"systems.txt") (if-exists :rename-and-delete))
  (when (eq system-names :here)
    (setf system-names (find-directory-systems)))
  (with-open-file (out outfile :direction :output :if-exists if-exists)
    (dolist (dep (collect-dependency-urls system-names))
      (write-line dep out)))
  (probe-file outfile))

(defun write-init-file (&optional directory)
  (uiop:copy-file (asdf:system-relative-pathname "project-loader" "init.lisp")
                  (merge-pathnames "init.lisp" (or directory
                                                   (uiop:getcwd)))))
