;;;; project-loader.lisp

(defpackage #:project-loader
  (:nicknames #:pl)
  (:use :cl)
  (:export
   #:initialize-registry
   #:pin-dependencies
   #:write-init-file))

(in-package #:project-loader)

(defun initialize-registry (&optional init-file)
  (let ((init (or init-file (merge-pathnames "init.lisp" (uiop:getcwd)))))
    (if (probe-file init)
        (load init)
        (format t "File not found: ~A" init))))

#+quicklisp
(defun collect-dependency-urls (system-names)
  "Collect the QL release URL for each dependency of SYSTEM"
  (flet ((archive-url (system)
           (ql-dist:archive-url (ql-dist:release system)))
         (archive-md5 (system)
           (ql-dist:archive-md5 (ql-dist:release system))))
    (labels ((collect (systems systems-table)
               (cond
                 ((null systems)
                  systems-table)
                 ((and (listp (first systems))
                       (eq :feature (caar systems)))
                  (collect (append (rest (first systems))
                                   (rest systems))
                    systems-table))
                 ((listp (first systems))
                  (collect (rest systems) systems-table))
                 ((asdf:find-system (first systems) nil)
                  (uiop:if-let ((ql-system (ql-dist:find-system (first systems))))
                    (setf (gethash (archive-url ql-system) systems-table)
                          (archive-md5 ql-system)))
                  (collect (append (asdf:system-depends-on (asdf:find-system (first systems)))
                                   (rest systems))
                    systems-table))
                 (t
                  (collect (rest systems) systems-table)))))
      (let ((systems-table (make-hash-table :test 'equal)))
        (collect system-names systems-table)
        systems-table))))

#+quicklisp
(defun pin-dependencies (system-names &key (outfile #p"ql.lock") (if-exists :rename-and-delete))
  (unless (listp system-names)
    (setf system-names (list system-names)))
  (with-open-file (systems outfile :direction :output :if-exists if-exists)
    (maphash (lambda (url hash)
               (format systems "~A ~A ~A~%"
                       (ppcre:regex-replace "http://" url "https://")
                       hash
                       (ppcre:scan-to-strings "[^/]+\.tgz" url)))
             (collect-dependency-urls system-names)))
  (probe-file outfile))

(defun write-init-file (&optional directory)
  (uiop:copy-file (asdf:system-relative-pathname "project-loader" "init.lisp")
                  (merge-pathnames "init.lisp" (or directory
                                                   (uiop:getcwd)))))
