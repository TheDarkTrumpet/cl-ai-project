; main.lisp
; Main file for what we'll use as the glue for this project
;

(in-package :ai)

(defun analyze-results (testing-results testing-set &key (class-index 0))
  "This function takes a results list and a testing list as well as an optional class-index. We loop through each element of the lists, and we check to see if the elements are equal - if they are, then we increment the correct-classifications else we increment the incorrect-classifications.  Keep in mind the normal type of input we're getting is:
testing-results: (...
                 ((class1 . classp) ... (classN . classp))
                 ...)
 where class1 is a string, and classp is a floating point number less than 1 that represents the probability.  Since we're concerned with only the first element since it's ordered by the classification algorithm, we take just the first element.
testing-set: (...
              (attrib0 attrib1 attrib2 ... attribN)
              ...)
where attrib-X defined as the class-index is our class variable that's used in comparison"
  (let ((correct-classifications 0)
	(incorrect-classifications 0))
    (loop for x in testing-results
	 for y in testing-set
	 do (if (equalp (car (first x))
			(elt y class-index))
		(incf correct-classifications)
		(incf incorrect-classifications)))
    (values (/ correct-classifications (+ correct-classifications incorrect-classifications) 1.0)
	    (with-output-to-string (s)
	      (format s "Correct Classifications: ~a, ~a~%Incorrect Classifications: ~a, ~a"
		      correct-classifications (/ correct-classifications (+ correct-classifications incorrect-classifications) 1.0)
		      incorrect-classifications (/ incorrect-classifications (+ correct-classifications incorrect-classifications) 1.0))))))

(defun display-results (results)
  "This function takes a list of lists (2 elements in the inner list) which is the result of calling analyze-results.  We output out the presentation layer of what we came up with, including an average percentage and the details.  See run-nb's documentation for example output"
  (format t "Classification algorithm finished:
Accuracy Listing: ~{~a, ~}
Accuracy Average: ~a
----------
Details returned:
~{~a~%~}"
	  (mapcar #'first results)
	  (/ (apply #'+ (mapcar #'first results)) (length results))
	  (mapcar #'second results)))

;To run run-nb, which should be run as a basis of the naive-bayes algorithm grading
;(require :ai)
;(in-package :ai)
;(run-nb 10)
(defun run-nb (folds)
  "Pulls in the data set, class variables, splits it into 66% and 33% lengths.  Then, for each fold we create a
random start varaible in the first 66% of the data, of that 33% of that goes into testing (using subseq, so
this is not random as far as the data that goes into this list).  The rest of the data does into a training
list.  From that, we bootstrap the training, class vars, and attribute vars.  We then call nb on the testing set,
which will return a list of ordered class probabilities - which given that and the test set we pass to
analyze results which will return two values - the first probability correct, and the second is a verbose message.
We do this folds-number times pulling it into results.  At the end, we call display-results with this data.
Output from this function looks something like the following, but only a small part of it:
AI> (run-nb 1)
Classification algorithm finished:
Accuracy Listing: 0.99927616, 
Accuracy Average: 0.99927616
----------
Details returned:
Correct Classifications: 2761, 0.99927616
Incorrect Classifications: 2, 7.238509e-4
NIL
"
  (let* ((data-set (getDataSet))
	 (attributes (getAttributeVariables))
	 (classvars (getClassVariables))
	 (66pc (floor (* (length data-set) .66)))
	 (33pc (- (length data-set) 66pc))
	 )
    (loop for x upto (- folds 1)
       for random-start-var = (random 66pc)
       for testing-set = (subseq data-set random-start-var (+ random-start-var 33pc))
       for training-set = (remove-if #'identity data-set :start random-start-var :count 33pc)
	 collect (progn
		   (ai-bayes::bootstrap :class-var-index 0 :training-set training-set :class-vars classvars :attribute-vars attributes)
		   (multiple-value-bind (x y) (analyze-results (ai-bayes::nb testing-set) testing-set)
		     (list x y))) into results
	 finally (display-results results))))

(defun get-nn-class-element (results)
  (let ((max-class nil)
	(max-class-count 0))
    (dolist (y (remove-duplicates (mapcar #'first results)))
      (if (> (count-if #'(lambda (x) (equalp (car x) y)) results) max-class-count)
	  (progn
	    (setf max-class-count (count-if #'(lambda (x) (equalp (car x) y)) results))
	    (setf max-class y))))
    max-class))

(defun analyze-nn-results (testing-results testing-set &key (class-index 0))
  (let ((correct-classifications 0)
	(incorrect-classifications 0))
    (loop for x in testing-results
	 for y in testing-set
	 do (progn
	      ;(format t "~a|~a : ~a|~a => ~a~%" (get-nn-class-element x) (type-of (get-nn-class-element x))
	;	      (elt y class-index) (type-of (elt y class-index)) (equalp (get-nn-class-element x) (elt y class-index)))
	      (if (equalp (get-nn-class-element x)
			(elt y class-index))
		(incf correct-classifications)
		(incf incorrect-classifications))) )
    (values (/ correct-classifications (+ correct-classifications incorrect-classifications) 1.0)
	    (with-output-to-string (s)
	      (format s "Correct Classifications: ~a, ~a~%Incorrect Classifications: ~a, ~a"
		      correct-classifications (/ correct-classifications (+ correct-classifications incorrect-classifications) 1.0)
		      incorrect-classifications (/ incorrect-classifications (+ correct-classifications incorrect-classifications) 1.0))))))

(defun run-nn (folds &key (threshold .01) (k 5))
  "Pulls in the data set, class variables, splits it into 66% and 33% lengths.  Then, for each fold we create a
random start varaible in the first 66% of the data, of that 33% of that goes into testing (using subseq, so
this is not random as far as the data that goes into this list).  The rest of the data does into a training
list.  From that, we bootstrap the training, class vars, and attribute vars.  We then call nb on the testing set,
which will return a list of ordered class probabilities - which given that and the test set we pass to
analyze results which will return two values - the first probability correct, and the second is a verbose message.
We do this folds-number times pulling it into results.  At the end, we call display-results with this data.
Output from this function looks something like the following, but only a small part of it:
AI> (run-nb 1)
Classification algorithm finished:
Accuracy Listing: 0.99927616, 
Accuracy Average: 0.99927616
----------
Details returned:
Correct Classifications: 2761, 0.99927616
Incorrect Classifications: 2, 7.238509e-4
NIL
"
  (let* ((data-set (getDataSet))
	 (attributes (getAttributeVariables))
	 (classvars (getClassVariables))
	 (66pc (floor (* (length data-set) .66)))
	 (33pc (- (length data-set) 66pc))
	 )
    (loop for x upto (- folds 1)
       for random-start-var = (random 66pc)
       for testing-set = (subseq data-set random-start-var (+ random-start-var 33pc))
       for training-set = (remove-if #'identity data-set :start random-start-var :count 33pc)
	 collect (progn
		   (format t "Running fold #:~a~%" (1+ x))
		   (finish-output)
		   (ai-nn::bootstrap :class-var-index 0 :training-set training-set :class-vars classvars :attribute-vars attributes :k k :threshold threshold)
		   (multiple-value-bind (x y)
		     (analyze-nn-results (ai-nn::nn testing-set) testing-set)
		     (list x y))) into results
	 finally (return (display-results results)))))


;;;;;;; RUN BOTH ;;;;;;;;
(defun run-everything (&key (folds 10))
  "This functions runs everything that goes into the whole performance data files.  It's fairly simple, all it does is iterate through each CSV, does NB, then for k = 1, k = 5, threshold = 1, .1, 01, and .001 then will run nearest neighbors - producting stats as it goes.  This process takes a LONG time,e ven with folds being very small.  This will either give apparent or true error results, with a special branch (that's not included) being used for the apparent errors cine the testing set assignment above is to the whole data set."
  (dolist (x '(#P"data/mushrooms/agaricus-lepiota.csv"
	       #P"data/traffic/vehicles.csv"))
    (format t "Processing CSV: ~a -- Naive Bayes~%" x)
    (clear-all-variables)
    (setf *data-set-file* x)
    (run-nb folds)
    (clear-all-variables)
    (setf *data-set-file* x)
    (format t "~%~%")
    (dolist (k-values '(1 5))
      (dolist (threshold-val '(1 .1 .01 .001))
	(format t "Processing CSV: ~a -- Nearest Neighbors with, k=~a and threshold=~a~%" x k-values threshold-val)
	(run-nn folds :threshold threshold-val :k k-values)
	(format t "~%~%")))))
