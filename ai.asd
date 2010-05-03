; AI Project File
;
; This is the main definition file that pulls together the algorithms for both Naive Bayes and K-Nearest Neighbors
; for the purpose of classifying instances.
;
; Programmer: David Thole
; Spring 2010 - AI

(in-package #:cl-user)

(defpackage #:ai-asd)

(in-package :ai-asd)

(asdf:defsystem :ai
  :name "AI Project File"
  :version "1.0"
  :author "David Thole"
  :license "LGPL"
  :description "This is the glue and what makes the plugin files actually work"
  :components ((:file "lib/package")
	       (:file "lib/variables")
	       (:file "lib/utils")
	       (:file "lib/main"))
  :depends-on (:fare-csv :ai-bayes :ai-nn)
  :serial t)