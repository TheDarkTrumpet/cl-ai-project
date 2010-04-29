; Naive Bayes Specific Learner
;

(in-package :cl-user)

(in-package :ai-bayes)

(defvar n-class nil)

(defvar p-class nil)

(defun count-all-attributes-in-classes ()
  