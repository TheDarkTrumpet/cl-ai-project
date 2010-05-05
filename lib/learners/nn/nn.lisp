; Nearest Neighbors Specific Learner
;
; Much of the code needed here is similar to that in the nb.lisp, so we'll be pulling data from there into here as we go

(in-package :ai-nn)

; Temporary holding areas for our number and probability
(defvar *n-class* nil)
(defvar *p-class* nil)

; Temporary holding areas for our data (to make things easier, maybe we can avoid
; duplication later...
(defvar *cv* nil)        ;cached variables
(defvar *cf* nil)        ;cached data set (training)
(defvar *av* nil)        ;cached attributes
(defvar *cfi* nil)       ;The class index
(defvar *k* nil)         ;The number of nodes to take into consideration in the ranking
(defvar *threshold* nil) ;Our threshold variable, which is used in the modified classify-testing-element.

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

(defun bootstrap (&key k class-var-index training-set class-vars attribute-vars threshold)
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
  (setf *threshold* threshold)
  t)

;******************************** INEFFICIENCY NOTE ************************************
;Note:
;This function listed below, along with the nn associated with it right below this are INEFFICIENT. I'm leaving the code here as a bit to explaining likely the 
;perfect solution way of going about this.  If we take into account every element in the data set, we are guaranteed to find their nearest neighbors.  With 8000
;elements, though, this is pretty much stupid..not impossible, but 10-fold on this large of a set takes awhile.
;
;I'm leaving the code here as a demo, but below this set of code you'll find the exact same functions, that include a weight.  These are passed in the bootstrap
;process, so I don't need to change the run-nn function from main.lisp in a way that would depend on these.  You can run one over the other if you *really* want to by simply
;commenting out the relevant sections (use ;) and uncommenting out the other sections.  Needless to say this took awhile, and I got tired of waiting after 5m"
;

;(defun classify-testing-element (acp example &key (training-set *cf*) (class-index *cfi*) (class-variables *cv*) (k *k*))
;  "Given an acp, that being an attribute-class-probability list, a specific example, and optional training set, class index, and class vars,
;we go through each element in the training set, finding out the distance of that element to our example pass in.  We loop through all elements
;in our training set doing this - at the end, we do a sort on the elements by ascending order taking only the first k values.
;acp => Our attribute-class-probability list of a-lists.
;example => Our testing example (assumes our class variable is still present, hence the class-index)
;Example output:
; AI-NN> (classify-testing-element (attribute-class-probability) *z* :training-set (subseq *cf* 0 10))
; ((\"poisonous\" . 0.0) (\"poisonous\" . 0.036825806) (\"poisonous\" . 0.2145414)
;  (\"edible\" . 0.35535172) (\"edible\" . 0.36859018))
;"
;  (loop for x in training-set collecting 
;       (cons (elt x class-index) (vector-distance (training-testing-to-vector acp example x :class-index class-index :class-variables class-variables))) into p-examples
;       finally (return (subseq (stable-sort p-examples #'(lambda (x y) (if (> (cdr x) (cdr y)) nil t))) 0 k))))


;******************************** MORE EFFICIENT NOTE ************************************
;The function below for classify-testing-element will basically act as exactly the same above, but we'll keep track of the threshold and if the values sit within that threshold, we'll
;store the values into a special variable.  The point of this is so that when our threshold reaches k, we can say we're done and just return the stable sort on those elements.
;
;This has a benefit of us possibly ending far sooner than we would have otherwise, but a problem that our error rate can increase quite a bit if we're not careful.  The right balance is nessary
;for us to actually finish this off...

(defun classify-testing-element (acp example &key (training-set *cf*) (class-index *cfi*) (class-variables *cv*) (k *k*) (tf *threshold*))
  "Given an acp, that being an attribute-class-probability list, a specific example, and optional training set, class index, and class vars,
we go through each element in the training set, finding out the distance of that element to our example pass in.  We loop through all elements
in our training set doing this - at the end, we do a sort on the elements by ascending order taking only the first k values.
acp => Our attribute-class-probability list of a-lists.
example => Our testing example (assumes our class variable is still present, hence the class-index)
Example output:
 AI-NN> (classify-testing-element (attribute-class-probability) *z* :training-set (subseq *cf* 0 10))
 ((\"poisonous\" . 0.0) (\"poisonous\" . 0.036825806) (\"poisonous\" . 0.2145414)
  (\"edible\" . 0.35535172) (\"edible\" . 0.36859018))
"
  (let ((my-special-var nil))
    (loop for x in training-set collecting 
	 (progn
	   (let* ((computed-example (vector-distance (training-testing-to-vector acp example x :class-index class-index :class-variables class-variables)))
		  (ret-val (cons (elt x class-index) computed-example)))
	     (if (<= computed-example tf)  ;We have a computed example in our threshold
		 (progn
		   (setf my-special-var (append my-special-var (list ret-val)))
		   (if (>= (length my-special-var) k)
		       (progn
			 (return my-special-var)))
		   ret-val)
		 ret-val)))
       into p-examples
       finally (return (subseq (stable-sort p-examples #'(lambda (x y) (if (> (cdr x) (cdr y)) nil t))) 0 k)))))
		       



;Below is an alternate way of defining nn.  What we'll do is accept a threshold of sorts.  If it is within this threshold, we'll put this into a special variable. When the count > our k, we'll
;just return that.  The benefit here is that we can likely end much sooner than we normally would.  The negative is that we can increase our error rate  if this threshold is set too high.  If it's
;too low, we'll basically just do nn like above...

(defun nn (testing-set)
  "This function takes a list of testing elements, and runs them throgh classify-testing-element for each particular element, and returns a list of ordered 
classifications."
  (when (or (null *cfi*) (null *cf*) (null *cv*) (null *av*) (null *cfi*) (null *k*))
    (error "You must call bootstrap before calling nn"))
  (let ((acp (attribute-class-probability)))
    (loop for x in testing-set 
       for y = 1 then (1+ y)
       collecting 
	 (classify-testing-element acp x)
       into testing-elements finally (return testing-elements))))

    