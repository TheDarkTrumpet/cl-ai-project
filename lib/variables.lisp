; variables.lisp
; Since the package is designed to be just our exports, and utils is our utility functions,
; we need a bit of a middleware for our global variables.
; Strictly speaking, we shouldn't really need this, if we deal in the pure functional-way, but
; to make things a little easier to understand, I'll make use of some global variables.

(in-package :ai)

(defvar *data-set-file* #P"/Users/dthole/programming/common-lisp/cl-ai-project/Data\ Set/agaricus-lepiota.csv")

(defvar *cached-variables* nil
  "Used by the with-all-variables macro to cache the variables upon reading of the file.  It increases speed on something not changed that much")

(defvar *cached-data-set* nil
  "Used by loadEntireDataSet to cache the data into this variable")