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
  "Groups up all the class counts, by using a temporary variable class-count to develop the organization such as:
((\"poisonous\" . 3916) (\"edible\" . 4208))
which is used later in the class-probability"
  (let ((class-count (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (line data-set)
      (incf (cdr (assoc (elt line class-index) class-count :test #'equalp))))
    (setf *n-class* class-count)
    class-count))

(defun class-probability (class-index &key (data-set *cf*) (class-variables *cv*))
  "Groups up all the class probabilities for this data set, by using a similar method as in count-all-attributes-in-classes.
Returns something such as:
((\"poisonous\" . 0.48202854) (\"edible\" . 0.51797146))
which is used later in doing the priors"
  (if (null *n-class*)
      (count-all-attributes-in-classes class-index data-set class-variables))
  (let ((class-probability (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (class *n-class*)
      (setf (cdr (assoc (car class) class-probability :test #'equalp)) 
	    (/ (cdr class) (length data-set) 1.0)))
    (setf *p-class* class-probability)
    class-probability))

(defun attribute-class-counter (class-index &key (data-set *cf*) (class-variables *cv*) (attribute-variables *av*))
  "This extremely overcomplicated function basically takes an index, and goes through the entire data set.  For each class 
of the row in that data set, we count the attributes affected by that class.  So, if row 1 has 5 attributes and a class of \"moo\",
then we increment, within those attribute values the class value moo, by 1.  Basically something we'll be returned as would be:
 (...
 ((\"urban\" (\"poisonous\" . 272) (\"edible\" . 96))
  (\"grasses\" (\"poisonous\" . 740) (\"edible\" . 1408))
  (\"meadows\" (\"poisonous\" . 36) (\"edible\" . 256))
  (\"woods\" (\"poisonous\" . 1268) (\"edible\" . 1880))
  (\"paths\" (\"poisonous\" . 1008) (\"edible\" . 136))
  (\"waste\" (\"poisonous\" . 0) (\"edible\" . 192))
  (\"leaves\" (\"poisonous\" . 592) (\"edible\" . 240)))
  ...)
where this one section represents *one* attribute column for our data set.  Each of these are values that can occur in that particular attribute.
The number of entries, totalled would quate out to the total number of entries in our data set provided.  The purpose of this is to develop probabilities later"
  (let ((acc (mapcar 
	      (lambda (x)
		(mapcar
		 (lambda (y) (cons y (mapcar 
					    (lambda (z) (cons z 0)) class-variables))) x))
	      attribute-variables)))
    (dolist (line data-set)
      (let ((aline (remove-if #'identity line :count 1 :start class-index :end (1+ class-index)))
	    (classv (elt line class-index)))
	(loop for attribute in aline
	   for x = 0 then (1+ x)
	   do
	   (incf (cdr (assoc classv (cdr (assoc attribute (elt acc x) :test #'equalp)) :test #'equalp))))))
    acc))


(defun bootstrap (data-set class-vars attribute-vars)
  "This bootstrap method is 1 of a few functions really called from the outside world.  The goal of this is to set
some public variables to make life a little easier in debugging.  These will likely remain, although you can call any
of the functions in this file scoped with special data.  Bootstrap just makes this more simple"
  (setf *cf* data-set)
  (setf *cv* class-vars)
  (setf *av* attribute-vars)
  t)

(defun nb ()
  "Our only exposed naive-bayes function that'll perform the training")


