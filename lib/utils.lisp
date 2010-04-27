; utils.lisp
; These are the glue-functions to make things a little easier, such as pulling out the class values, attribute values, and so on.

(in-package :ai)

(defmacro with-data-file ((s file) &body body)
  `(with-open-file (,s ,file :direction :input :if-does-not-exist :error)
     ,@body))

(defun getClassVariables (&key (index 0) (file *data-set-file*))
  "Given an a default index 0 (first column), we parse the data set file to determine all the possible value options for our class."
  
  )