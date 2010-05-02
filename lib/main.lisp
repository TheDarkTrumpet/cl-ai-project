; main.lisp
; Main file for what we'll use as the glue for this project
;

(in-package :ai)


"Classification algorithm:  Need algorithm name, chunk, eval-function"

(defun analyze-results ())

(defun run-nb (folds)
  (let ((data-set (getDataSet))
	(attributes (getAttributeVariables))
	(classvars (getClassVariables)))
    (loop for x upto (- folds 1)
       for y = (random (floor (* (length data-set) .66)))
       for training-set = (subseq data-set y (- (length data-set) y))
       for testing-set = (remove-if #'identity data-set :start y :count (- (length data-set) y))
	 collect (progn
		   (ai-bayes::bootstrap :class-var-index 0 :testing-set testing-set :class-vars classvars :attribute-vars attributes)
		   (analyze-results)))))
  (loop for x = 1 upto 10 do
  (ai-bayes::bootstrap :class-var-index 0 :testing-set (getDataSet) :class-vars (getClassVariables) :attribute-vars (getAttributeVariables))
  (ai-bayes::nb (subseq (getDataSet) 0 10)))

; To test and debug:
; (in-package :ai)
; (ai-bayes::bootstrap :class-var-index 0 :testing-set (getDataSet) :class-vars (getClassVariables) :attribute-vars (getAttributeVariables))
; (in-package :ai-bayes)