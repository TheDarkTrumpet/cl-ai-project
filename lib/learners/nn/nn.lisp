; Nearest Neighbors Specific Learner
;
; Much of the code needed here is similar to that in the nb.lisp, so we'll be pulling data from there into here as we go

(in-package :ai-nn)

; Temporary holding areas for our number and probability
(defvar *n-class* nil)
(defvar *p-class* nil)

; Temporary holding areas for our data (to make things easier, maybe we can avoid
; duplication later...
(defvar *cv* nil)  ;cached variables
(defvar *cf* nil)  ;cached data set (training)
(defvar *av* nil)  ;cached attributes
(defvar *cfi* nil) ;The class index
(defvar *k* nil)   ;The number of nodes to take into consideration in the ranking


(defun attribute-class-probability (&key (class-variables *cv*))
  (ai-bayes::bootstrap :class-var-index 0 :training-set *cf* :class-vars *cv* :attribute-vars *av*)
  (ai-bayes::attribute-class-probability :class-variables class-variables))

(defun vector-distance (dist-list)
  "Given a list of distances as a list, we compute a single number that represents the distance overall.  This is from one node
to another"
  (loop for x in dist-list summing (expt x 2) into distance finally (return distance)))

(defun chop-class-off-training (vector-val class-index)
  "Given a vector and class val, all we do is chop off the first element of that list - this can likely go in utils."
  (remove-if #'identity vector-val :start class-index :end (1+ class-index)))

(defun training-testing-to-vector (acp example training-example &key (class-variables *cv*) (class-index *cfi*))
  "Given two lists of attribute variables, we take a technique given from the following site:
http://horicky.blogspot.com/2009/05/machine-learning-nearest-neighbor.html
I like the concept but I changed it some.  Given two vectors, we find the absolute value of the subtraction of each element's
probability given the class.  His specific calculation: (xai - xbi) = sum_over_k(P(class[k] | xai) – P(class[k] | xbi)).
We have the P(class[k] given from the acp used in the bayes algorithm - the subtraction is done for that specific class and the absolute
value is taken.  I decided on the absolute value because I sum these over again in vector-distance, which since I'm going to square them,
this isn't really needed.  Leaving it anyways since it's more clear that there still is a distance.
acp => Our attribute-class-probability list of a-lists.
example => Our testing example*
training-example => Our training example*
* Note that this assumes the class value is included, which we chop off at the beginning!
Example output:
 (training-testing-to-vector (attribute-class-probability) *z* *x*)
 (0.0 0.0 0.1491876 0.0 0.0807484 0.0 0.0 0.5125554 0.0 0.0 0.06942393 0.0 0.0
 0.0 0.0 0.0 0.0 0.0 0.0 0.011816844 0.10438208 0.21910389)"
  (loop for x in (chop-class-off-training example class-index)
       for y in (chop-class-off-training training-example class-index)
       for p-attrib in acp
       for z = 0 then (1+ z)
       collect (loop for class in class-variables summing
		    (abs (- (cdr (assoc class (cdr (assoc x p-attrib :test #'equalp)) :test #'equalp))
			    (cdr (assoc class (cdr (assoc y p-attrib :test #'equalp)) :test #'equalp)))))))

(defun classify-testing-element (acp example &key (training-set *cf*) (class-index *cfi*) (class-variables *cv*))
  "Given an acp, that being an attribute-class-probability list, a specific example, and optional training set, class index, and class vars,
we go through each element in the training set, finding out the distance of that element to our example pass in.  We loop through all elements
in our training set doing this - at the end, we do a sort on the elements.

Should we do the chopping off here and grouping?  Majority wins, but should that be done here?"
  (loop for x in training-set 
  )

(defun bootstrap (&key k class-var-index training-set class-vars attribute-vars)
  "This bootstrap method is 1 of a few functions really called from the outside world.  The goal of this is to set
some public variables to make life a little easier in debugging.  These will likely remain, although you can call any
of the functions in this file scoped with special data.  Bootstrap just makes this more simple.
Arguments:
class-var-index => the index of the class varaible in our training set
training-set => The list of training elements
class-vars => The listing of possible class variables
attribute-vars => The listing of possible attribute-variables
k => The number of nodes to consider for what something gets classified as."
  (setf *k* k)
  (setf *cfi* class-var-index)
  (setf *cf* training-set)
  (setf *cv* class-vars)
  (setf *av* attribute-vars)
  t)

(defun nn (testing-set)
  "This function takes a list of testing elements, and runs them throgh classify-testing-element for each particular element, and returns a list of ordered 
classifications."
  (when (or (null *cfi*) (null *cf*) (null *cv*) (null *av*) (null *cfi*) (null *k*))
    (error "You must call bootstrap before calling nn"))
  (let ((acp (attribute-class-probability)))
    (loop for x in testing-set collecting (classify-testing-element acp x) into testing-elements finally (return testing-elements))))