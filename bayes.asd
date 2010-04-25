; Naive Bayes Project File
;
; Programmer: David Thole
; Spring 2010 - AI

(in-package #:cl-user)

(defpackage #:bayes-asd)

(in-package :bayes-asd)

(asdf:defsystem :ai-bayes
  :name "Naive Bayes Plugin for the AI project"
  :version "0.1"
  :author "David Thole"
  :license "LGPL"
  :description "This is the Naive Bayes learning algorithm for classification"
  :components ((:file "lib/learners/nb"))
  :serial t)
