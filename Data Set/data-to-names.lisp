; data-to-names.lisp
; This file's simple objective is to translate the shorthand CSV given by the original data set into something
; that's more intelligently written - that is, with the long-hand version of the attributes.  Weka was sad when
; I was trying to use this file.  My eventual algorithm will likely rely on this data set being converted in a 
; similar manner, for reporting purposes.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn (require :fare-csv)))


(defpackage :data-to-names
  (:use :fare-csv :cl))

(in-package :data-to-names)

(defvar *data-file-r* #P"/Users/dthole/programming/common-lisp/cl-k-nearest-neighbors/Data\ Set/agaricus-lepiota.data"
	"This is where the original data file is at, note - if you run this you'll likely need to change the path")

(defvar *data-file-w* #P"/Users/dthole/programming/common-lisp/cl-k-nearest-neighbors/Data\ Set/agaricus-lepiota.csv"
	"This is where the csv data file is to be written to, note - if you run this you'll likely need to change the path")

(setf *translation-layer*
  '(((E . "edible")      ;Class
     (P . "poisonous"))
    ((B . "bell")        ;1. Cap-shape
     (c . "conical")
     (X . "convex")
     (f . "flat")
     (k . "knobbed")
     (s . "sunken"))
    ((F . "fibrous")     ;2. Cap-Surface
     (G . "grooves")
     (Y . "scaly")
     (S . "smooth"))
    ((N . "brown")       ;3. Cap-color
     (B . "buff")
     (C . "cinnamon")
     (G . "gray")
     (R . "green")
     (P . "pink")
     (U . "purple")
     (E . "red")
     (W . "white")
     (Y . "yellow"))
    ((T . "bruises")     ;4  bruises?
     (F . "nobruises"))
    ((A . "almond")      ;5. odor
     (L . "anise")
     (C . "creosote")
     (Y . "fishy")
     (F . "foul")
     (M . "musty")
     (N . "none")
     (P . "pungent")
     (S . "spicy"))
    ((A . "attached")    ;6. Gill-attachment
     (D . "descending")
     (F . "free")
     (N . "notched"))
    ((C . "close")       ;7. Gill-spacing
     (W . "crowded")
     (D . "distant"))
    ((B . "broad")       ;8. Gill-size
     (N . "narrow"))
    ((K . "black")       ;9. Gill-color
     (N . "brown")
     (B . "buff")
     (H . "chocolate")
     (G . "gray")
     (R . "green")
     (O . "orange")
     (P . "pink")
     (U . "purple")
     (E . "red")
     (W . "white")
     (Y . "yellow"))
    ((E . "enlarging")   ;10. stalk-shape
     (T . "tapering"))
    ((B . "bulbous")     ;11. stalk-root
     (C . "club")
     (U . "cup")
     (E . "equal")
     (Z . "rhizomorphs")
     (R . "rooted")
     (? . "missing"))
    ((F . "fibrous")     ;12. stalk-surface-above-ring
     (Y . "scaly")
     (K . "silky")
     (S . "smooth"))
    ((F . "fibrous")     ;13. stalk-surface-below-ring
     (Y . "scaly")
     (K . "silky")
     (S . "smooth"))
    ((N . "brown")       ;14. stalk-color-above-ring
     (B . "buff")
     (C . "cinnamon")
     (G . "gray")
     (O . "orange")
     (P . "pink")
     (E . "red")
     (W . "white")
     (Y . "yellow"))
    ((N . "brown")       ;15. stalk-color-below-ring
     (B . "buff")
     (C . "cinnamon")
     (G . "gray")
     (O . "orange")
     (P . "pink")
     (E . "red")
     (W . "white")
     (Y . "yellow"))
    ((P . "partial")     ;16. veil-type
     (U . "universal"))
    ((N . "brown")       ;17. veil-color
     (O . "orange")
     (W . "white")
     (Y . "yellow"))
    ((N . "none")        ;18. ring-number
     (O . "one")
     (T . "two"))
    ((C . "cobwebby")    ;19. ring-type
     (E . "evanescent")
     (F . "flaring")
     (L . "large")
     (N . "none")
     (P . "pendant")
     (S . "sheathing")
     (Z . "zone"))
    ((K . "black")       ;20. spore-print-color
     (N . "brown")
     (B . "buff")
     (H . "chocolate")
     (R . "green")
     (O . "orange")
     (U . "purple")
     (W . "white")
     (Y . "yellow"))     ;21. population
    ((A . "abundant")
     (C . "clustered")
     (N . "numerous")
     (S . "scattered")
     (V . "several")
     (Y . "solitary"))
    ((G . "grasses")     ;22. habitat
     (L . "leaves")
     (M . "meadows")
     (P . "paths")
     (U . "urban")
     (W . "waste")
     (D . "woods"))))
  "This is a direct translation of part 7 in the agaricus-lepiota.names file - yes, this was painful to do manually")

(defun process-translation (csvline)
  "Given a csvline, that is a list from our cvs parser, we'll loop through each element, and loop through each element in *translation-layer*
doing a 1-1 conversion of the data"
  (loop for x in csvline
     for y in *translation-layer*
       collect (cdr (assoc
		     (intern (string-upcase x))
		     y))))

(defun data-to-names ()
  "Opens the .data file, and creates the same as a csv, doing a lookup in our table defined under *names-attributes* and does a replace for the shorthand to the longhand version.
The purpose of doing this is to not make weka sad when it comes to testing what's a good algorithm to use."
  (with-open-file (r *data-file-r* :direction :input :if-does-not-exist :error)
    (with-open-file (w *data-file-w* :direction :output :if-does-not-exist :create :if-exists :overwrite)
      (loop for line = (read-csv-line r)
	 for pline = (process-translation line)
	 while line
	 do 
	   (write-csv-line pline w)))))
