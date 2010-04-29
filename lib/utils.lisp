; utils.lisp
; These are the glue-functions to make things a little easier, such as pulling out the class values, attribute values, and so on.

(in-package :ai)

(defmacro with-data-file ((s file) &body body)
  `(with-open-file (,s ,file :direction :input :if-does-not-exist :error)
     ,@body))

(defun getAllVariables (&key (file *data-set-file*))
  "Parses through the entire data-set file grabbing all the variables.  The one issue is this is pretty inefficient in terms of memory, we load everything,
then remove duplicates afterwards"
  (with-data-file (s file)
    (let* ((fline (read-csv-line s))
	   (allvars (map 'list #'(lambda (x) (list x)) fline)))
      (loop for line = (read-csv-line s) while line do
	   (loop for x = 0 then (+ 1 x) for cols in line do
		(setf (elt allvars x) (append (elt allvars x) (set-difference (list cols) (elt allvars x) :test #'string-equal)))) finally (return allvars) ))))

(defun getClassVariables (&key (index 0) (file *data-set-file*))
  "Given an a default index 0 (first column), we parse the data set file to determine all the possible value options for our class."
  (with-data-file (s file)
    (loop for line = (read-csv-line s) while line collecting (elt line index) into class-vars finally (return (delete-duplicates class-vars :test #'string-equal)))))

(defun getAttributeVariables (&key (class-index 0) (file *data-set-file*))
  "Given a class index, we'll parse all the other indexes, but that one, removing all the duplicates in the end.")