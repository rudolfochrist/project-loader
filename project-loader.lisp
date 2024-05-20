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
         (archive-md (system)
           (ql-dist:archive-md5 (ql-dist:release system))))
    (labels ((collect (systems systems-table)
               (let ((system-name (print (first systems))))
                 (when (and system-name
                            (not (consp system-name)))
                   (let ((asdf-system (asdf:find-system system-name nil))
                         (ql-system (ql-dist:find-system system-name)))
                     (when ql-system
                       (setf (gethash (archive-url ql-system) systems-table)
                             (archive-md ql-system)))
                     (collect (asdf:system-depends-on asdf-system) systems-table)
                     (collect (rest systems) systems-table))))))
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
