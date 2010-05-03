; main.lisp
; Main file for what we'll use as the glue for this project
;

(in-package :ai)


"Classification algorithm:  Need algorithm name, chunk, eval-function"

(defun analyze-results (testing-results testing-set)
  (format t "~a : ~a~%" (first testing-results) (first testing-set)))

(defun run-nb (folds)
  (let ((data-set (getDataSet))
	(attributes (getAttributeVariables))
	(classvars (getClassVariables)))
    (loop for x upto (- folds 1)
       for y = (random (floor (* (length data-set) .66)))
       for training-set = (subseq data-set y (+ y (- (length data-set) y)))
       for testing-set = (remove-if #'identity data-set :start y :count (- (length data-set) y))
	 collect (progn
		   (ai-bayes::bootstrap :class-var-index 0 :training-set training-set :class-vars classvars :attribute-vars attributes)
		   (analyze-results (ai-bayes::nb testing-set) testing-set)))))

; To test and debug:
; (in-package :ai)
; (ai-bayes::bootstrap :class-var-index 0 :testing-set (getDataSet) :class-vars (getClassVariables) :attribute-vars (getAttributeVariables))
; (in-package :ai-bayes)