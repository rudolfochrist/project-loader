1 NAME
======

  project-loader --- A utility to load Quicklisp systems without
  Quicklisp itself


2 VERSION
=========

  ,----
  | 0.2.0
  `----


3 SYNOPSIS
==========

  ,----
  | (project-loader:create-load-file "/path/")
  `----

  Later..

  ,----
  | $ sbcl --no-userinit --load load.lisp
  `----


4 DESCRIPTION
=============

  Dump images or create executables without Quicklisp but still find
  your dependencies.


4.1 FUNCTIONS
~~~~~~~~~~~~~

  `PROJECT-LOADER:CREATE-LOAD-FILE (&OPTIONAL PATH)'
        Create load.lisp file at PATH.  If PATH is nil use current
        working directory.


5 AUTHOR
========

  Sebastian Christ (<mailto:rudolfo.christ@pm.me>)


6 LICENSE
=========

  Released under the MPL-2.0 license.
