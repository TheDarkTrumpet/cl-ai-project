; Nearest Neighbors Project File
;
; Programmer: David Thole
; Spring 2010 - AI

(in-package #:cl-user)

(defpackage #:nn-asd)

(in-package :nn-asd)

(asdf:defsystem :ai-nn
  :name "Nearest Neighbors Plugin for the AI project"
  :version "0.1"
  :author "David Thole"
  :license "LGPL"
  :description "This is the Nearest Neighbors learning algorithm for classification"
  :components ((:file "lib/learners/nn/package")
	       (:file "lib/learners/nn/nn"))
  :serial t)
