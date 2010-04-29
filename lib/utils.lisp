; utils.lisp
; These are the glue-functions to make things a little easier, such as pulling out the class values, attribute values, and so on.

(in-package :ai)

(defmacro with-data-file ((s file) &body body)
  `(with-open-file (,s ,file :direction :input :if-does-not-exist :error)
     ,@body))

(defmacro with-all-variables ((v &key (cachep t) (file *data-set-file*)) &body body)
  "Stores a temporary copy of the variable from getAllVariables into v, and we use the cached
variable, if it's available, if cachep is set. If cachep is nil, we'll reload the file and
will not store the cache"
  `(let ((,v nil))
     (cond
       (,cachep
	(if (null *cached-variables*)
	    (progn
	      (setf *cached-variables* (getAllVariables :file ,file))
	      (setf ,v (copy-seq *cached-variables*)))
	    (setf ,v (copy-seq *cached-variables*))))
       (t
	(setf ,v (getAllVariables :file ,file))))
     ,@body))

(defmacro with-data-set ((v *key (cachep t) (file *data-set-file*)) &body body)
  `(let ((,v nil))
     (cond
       (,cachep
	(if (null *cached-data-set*)
	    (progn
	      (setf *cached-data-set* (loadEntireDataSet :file ,file))
	      (setf ,v (copy-seq *cached-data-set*)))
	    (setf ,v (copy-seq *cached-data-set*))))
       (t
	(setf ,v (loadEntireDataSet :file ,file))))
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

(defun loadEntireDataSet (&key (file *data-set-file*))
  (let ((datas nil))
    (with-data-file (s file)
      (loop for line = (read-csv-line s) while line collect line into l finally (setf datas l)))
    datas))
	 
(defun getClassVariables (&key (index 0) (file *data-set-file*))
  "Given an a default index 0 (first column), we parse the data set file to determine all the possible value options for our class."
  (with-all-variables (vars :file file)
    (elt vars index)))

(defun getAttributeVariables (&key (class-index 0) (file *data-set-file*))
  "Given a class index, defauled to 0, we will remove that from all the variables and return.  Note that if class-index is given out of range, we won't really care and will return everything"
  (with-all-variables (vars :file file)
    (remove-if #'identity vars :count 1 :start class-index :end (1+ class-index))))
