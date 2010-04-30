; Naive Bayes Specific Learner
;

(in-package :cl-user)

(in-package :ai-bayes)

; Temporary holding areas for our number and probability
(defvar *n-class* nil)
(defvar *p-class* nil)

; Temporary holding areas for our data (to make things easier, maybe we can avoid
; duplication later...
(defvar *cv* nil) ;cached variables
(defvar *cf* nil) ;cached data set
(defvar *av* nil) ;cached attributes

(defun count-all-attributes-in-classes (class-index &key (data-set *cf*) (class-variables *cv*))
  (let ((class-count (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (line data-set)
      (incf (cdr (assoc (elt line class-index) class-count :test #'equalp))))
    (setf *n-class* class-count)
    class-count))

(defun class-probability (class-index &key (data-set *cf*) (class-variables *cv*))
  (if (null *n-class*)
      (count-all-attributes-in-classes class-index data-set class-variables))
  (let ((class-probability (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (class *n-class*)
      (setf (cdr (assoc (car class) class-probability :test #'equalp)) 
	    (/ (cdr class) (length data-set) 1.0)))
    (setf *p-class* class-probability)
    class-probability))

(defun bootstrap (data-set class-vars attribute-vars)
  (setf *cf* data-set)
  (setf *cv* class-vars)
  (setf *av* attribute-vars))

(defun nb ()
  "Our only exposed naive-bayes function that'll perform the training")


