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

(defun vector-distance (dist-list)
  "Given a list of distances as a list, we compute a single number that represents the distance overall.  This is from one node
to another"
  (loop for x in dist-list summing (expt x 2) into distance finally (return distance)))

(defun attribute-class-probability (&key (class-variables *cv*))
  (ai-bayes::bootstrap :class-var-index 0 :training-set *cf* :class-vars *cv* :attribute-vars *av*)
  (ai-bayes::attribute-class-probability :class-variables class-variables))

(defun classify-testing-element (acp example &key (training-set *cf*) (class-index *cfi*) (class-variables *cv*))
  "Given an acp, that being an attribute-class-probability list, a specific example, and optional training set, class index, and class vars,
we go through each element in the training set, finding out the distance of that element to our example pass in.  We loop through all elements
in our training set doing this - at the end, we do a sort on the elements.

Should we do the chopping off here and grouping?  Majority wins, but should that be done here?"
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