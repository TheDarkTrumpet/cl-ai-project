; Naive Bayes Specific Learner
;

(in-package :cl-user)

(in-package :ai-bayes)

(defvar *n-class* nil)

(defvar *p-class* nil)

(defun count-all-attributes-in-classes (class-index data-set class-variables)
  (let ((class-count (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (line data-set)
      (incf (cdr (assoc (elt line class-index) class-count :test #'equalp))))
    (setf *n-class* class-count)
    class-count))

(defun class-probability ()
  )

(defun nb ()
  "Our only exposed naive-bayes function that'll perform the training")


