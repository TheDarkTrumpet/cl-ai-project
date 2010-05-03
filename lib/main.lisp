; main.lisp
; Main file for what we'll use as the glue for this project
;

(in-package :ai)

(defun analyze-results (testing-results testing-set &key (class-index 0))
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
	      (format s "Correct Classifications: ~a, ~a~%Incorrect Classifications: ~a, ~a~%"
		      correct-classifications (/ correct-classifications (+ correct-classifications incorrect-classifications) 1.0)
		      incorrect-classifications (/ incorrect-classifications (+ correct-classifications incorrect-classifications) 1.0))))))

  (format t "~a : ~a~%" (first testing-results) (first testing-set)))

(defun run-nb (folds)
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
		   (format t "r: ~a x: ~a : ~a ~a~%" random-start-var x (length training-set) (length testing-set))
		   (ai-bayes::bootstrap :class-var-index 0 :training-set training-set :class-vars classvars :attribute-vars attributes)
		   (analyze-results (ai-bayes::nb testing-set) testing-set)))))

; To test and debug:
; (in-package :ai)
; (ai-bayes::bootstrap :class-var-index 0 :testing-set (getDataSet) :class-vars (getClassVariables) :attribute-vars (getAttributeVariables))
; (in-package :ai-bayes)