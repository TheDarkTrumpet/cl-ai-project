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

