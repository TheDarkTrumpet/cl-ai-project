; Naive Bayes Specific Learner
;

(in-package :ai-bayes)

; Temporary holding areas for our number and probability
(defvar *n-class* nil)
(defvar *p-class* nil)

; Temporary holding areas for our data (to make things easier, maybe we can avoid
; duplication later...
(defvar *cv* nil)  ;cached variables
(defvar *cf* nil)  ;cached data set (training)
(defvar *av* nil)  ;cached attributes
(defvar *cfi* nil) ;The class index

(defun class-count (&key (class-index *cfi*) (data-set *cf*) (class-variables *cv*))
  "Groups up all the class counts, by using a temporary variable class-count to develop the organization such as:
 ((\"poisonous\" . 3916) (\"edible\" . 4208))
 which is used later in the class-probability"
  (let ((class-count (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (line data-set)
      (incf (cdr (assoc (elt line class-index) class-count :test #'equalp))))
    (setf *n-class* class-count)
    class-count))

(defun class-probability (&key (class-index *cfi*) (data-set *cf*) (class-variables *cv*))
  "Groups up all the class probabilities for this data set, by using a similar method as in count-all-attributes- in-classes.
 Returns something such as:
 ((\"poisonous\" . 0.48202854) (\"edible\" . 0.51797146))
 which is used later in doing the priors"
  (if (null *n-class*)
      (class-count))
  (let ((class-probability (mapcar (lambda (x) (cons x 0)) class-variables)))
    (dolist (class *n-class*)
      (setf (cdr (assoc (car class) class-probability :test #'equalp)) 
	    (/ (cdr class) (length data-set) 1.0)))
    (setf *p-class* class-probability)
    class-probability))

(defun attribute-class-counter (&key (class-index *cfi*) (data-set *cf*) (class-variables *cv*) (attribute-variables *av*))
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

(defun attribute-class-probability (&key (class-variables *cv*))
"Given elements derived from the attribite-class-counter, we compute the probabilities of this attribute's class occurance by the following equation (taken from 
orange's naive bayes tutorial:
pc[i][j][k] = (pc[i][j][k] + self.m * p_class[k]) / (n_class[k] + self.m)
where pc is defined as our count from attribute-class-counter, m is not implemented here, p_class is the class probability and
n_class is the number of occurances for this class.  See functions class-count, and class-probability for what these would come out to.
A bit of example output would be something like:
 (...
 ((\"scattered\" (\"poisonous\" . 0.045297883) (\"edible\" . 0.108321026))
  (\"numerous\" (\"poisonous\" . 0.0) (\"edible\" . 0.04923683))
  (\"abundant\" (\"poisonous\" . 0.0) (\"edible\" . 0.047267355))
  (\"several\" (\"poisonous\" . 0.3505662) (\"edible\" . 0.14672576))
  (\"solitary\" (\"poisonous\" . 0.07976366) (\"edible\" . 0.13096997))
  (\"clustered\" (\"poisonous\" . 0.0064007877) (\"edible\" . 0.035450518)))
  ...)
"
  (let ((acc (attribute-class-counter))
	(p-class (class-probability))
	(n-class (class-count)))
    (loop for i in acc do
	 (loop for j in i do
	      (loop for k in class-variables do
		   (setf (cdr (assoc k (cdr j) :test #'equalp)) 
			 (/ (* (cdr (assoc k (cdr j) :test #'equalp)) (cdr (assoc k p-class :test #'equalp))) 
			    (cdr (assoc k n-class :test #'equalp)))))))
    acc))



;;;;;;; These functions below are called outside of this particular package.  They are for setup and the actual learning/testing process. ;;;;;
(defun bootstrap (&key class-var-index training-set class-vars attribute-vars)
  "This bootstrap method is 1 of a few functions really called from the outside world.  The goal of this is to set
some public variables to make life a little easier in debugging.  These will likely remain, although you can call any
of the functions in this file scoped with special data.  Bootstrap just makes this more simple"
  (setf *cfi* class-var-index)
  (setf *cf* training-set)
  (setf *cv* class-vars)
  (setf *av* attribute-vars)
  (setf *n-class* nil)
  (setf *p-class* nil)
  t)

(defun normalize-probability (pc)
  "This is a normalization function, we take in the pc derived from classify-testing-element, and if the sum is greater than 0, we normalize
the data back to under 0 so the probabilities make more sense.  We could likely just do this for even less than 0 - which would actually give more
reasonable probabilities since it doesn't equal 1 - but we'll deal with only stuff greater than 1.  I don't see where we'd need this, given the operations above
but the tutorial did it, and I don't see any harm in keeping it here.

Example output: 
 AI-BAYES> (normalize-probability '((\"poisonous\" . 0.5) (\"edible\" . 0.4)))
 ((\"poisonous\" . 0.5) (\"edible\" . 0.4))
 AI-BAYES> (normalize-probability '((\"poisonous\" . 0.5) (\"edible\" . 1.4)))
 ((\"poisonous\" . 0.2631579) (\"edible\" . 0.7368421))"
  (loop for x in pc 
     for y = (cdr x) summing y into sum finally
       (progn (when (> sum 1)
		(loop for y in pc do (setf (cdr y) (/ (cdr y) sum))))
	      (return pc))))
       
(defun classify-testing-element (acp example &key (class-index *cfi*) (class-variables *cv*))
  "Given acp, an attribute-class-probability list, and a specific example, we compute the P[Class_Variable], for this particular example.
The values returned is a list of elements, ordered such that the first element of the list is the a-list that had the greatest probability for this
class example.  The rest of the elements are the other class varaible and probability a-lists, with ordering such that the class with the greatest
probability resides closer to the front of the list.

Example output: 
 ((\"edible\" . 4.1844698e-16) (\"poisonous\" . 0.0))"
  (let ((aline (remove-if #'identity example :count 1 :start class-index :end (1+ class-index)))
	(pc (mapcar (lambda (x) (cons x 1.0)) class-variables)))
    (loop for t-attrib in aline
	 for p-attrib in acp do
	 (loop for class in class-variables do
	      (setf (cdr (assoc class pc)) (* (cdr (assoc class pc :test #'equalp))
					      (cdr (assoc class (cdr (assoc t-attrib p-attrib :test #'equalp)) :test #'equalp))))))
    (stable-sort (normalize-probability pc) #'(lambda (x y) (if (> (cdr x) (cdr y)) t nil)))))
  
(defun nb (testing-set)
  "This function takes a list of testing-set elements and runs them through classify-testing-element for each particular element, and returns a list of ordered
classifications.

Example output:
 AI-BAYES> (nb (subseq *cf* 0 10))
 (((\"poisonous\" . 2.8199686e-20) (\"edible\" . 0.0))
  ((\"edible\" . 2.265796e-15) (\"poisonous\" . 0.0))
  ((\"edible\" . 4.1844698e-16) (\"poisonous\" . 0.0))
  ((\"poisonous\" . 1.9078604e-20) (\"edible\" . 0.0))
  ((\"edible\" . 8.735333e-15) (\"poisonous\" . 0.0))
  ((\"edible\" . 7.658977e-15) (\"poisonous\" . 0.0))
  ((\"edible\" . 1.0476759e-16) (\"poisonous\" . 0.0))
  ((\"edible\" . 1.2102772e-15) (\"poisonous\" . 0.0))
  ((\"poisonous\" . 2.2954268e-18) (\"edible\" . 0.0))
  ((\"edible\" . 1.2804928e-16) (\"poisonous\" . 0.0)))
"
  (when (or (null *cfi*) (null *cf*) (null *cv*) (null *av*) (null *cfi*))
    (error "You must call bootstrap before calling nb"))
  (let ((acp (attribute-class-probability)))
    (loop for x in testing-set collecting (classify-testing-element acp x) into testing-elements finally (return testing-elements))))
