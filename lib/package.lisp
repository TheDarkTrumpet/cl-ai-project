(in-package :cl-user)

(defpackage :ai
  (:use :cl :fare-csv
	:ai-bayes)
  (:export :*data-set-file* ;Our global variable for the data set
	   ;Utility Functions
	   :getClassVariables))