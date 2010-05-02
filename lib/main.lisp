; main.lisp
; Main file for what we'll use as the glue for this project
;

(in-package :ai)


"Classification algorithm:  Need algorithm name, chunk, eval-function"


; To test and debug:
; (in-package :ai)
; (ai-bayes::bootstrap :class-var-index 0 :testing-set (getDataSet) :class-vars (getClassVariables) :attribute-vars (getAttributeVariables))
; (in-package :ai-bayes)