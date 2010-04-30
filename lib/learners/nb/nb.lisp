; Naive Bayes Specific Learner
;

(in-package :cl-user)

(in-package :ai-bayes)

(defvar *n-class* nil)

(defvar *p-class* nil)

(defun count-all-attributes-in-classes (class-index data-set class-variables)
  (let ((class-count (make-hash-table :size (length class-variables))))
    (dolist (class class-variables)
      (setf (gethash (intern class) class-count) 0))
    (dolist (line data-set)
      (incf (gethash (intern (elt line class-index)) class-count)))
    (setf *n-class* class-count)
    class-count))

(defun class-probability ()
  )

(defun nb ()
  "Our only exposed naive-bayes function that'll perform the training")


