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

(defun attribute-class-counter (class-index &key (data-set *cf*) (class-variables *cv*) (attribute-variables *av*))
  (let ((acc (mapcar 
	      (lambda (x)
		(mapcar
		 (lambda (y) (cons y (mapcar 
					    (lambda (z) (cons z 0)) class-variables))) x))
	      attribute-variables)))
    (dolist (line (list (first data-set)))
      (let ((aline (remove-if #'identity line :count 1 :start class-index :end (1+ class-index)))
	    (classv (elt line class-index)))
	(format t "~a : ~a~%" aline classv)
	(loop for attribute in aline
	   for x = 0 then (1+ x)
	   do
	   (incf (cdr(assoc classv (cdr (assoc attribute (elt acc x) :test #'equalp)) :test #'equalp))))))
    acc))


(defun bootstrap (data-set class-vars attribute-vars)
  (setf *cf* data-set)
  (setf *cv* class-vars)
  (setf *av* attribute-vars)
  t)

(defun nb ()
  "Our only exposed naive-bayes function that'll perform the training")


