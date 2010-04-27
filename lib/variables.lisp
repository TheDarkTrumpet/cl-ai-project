; variables.lisp
; Since the package is designed to be just our exports, and utils is our utility functions,
; we need a bit of a middleware for our global variables.
; Strictly speaking, we shouldn't really need this, if we deal in the pure functional-way, but
; to make things a little easier to understand, I'll make use of some global variables.

(defvar *data-set-file* #P"/Users/dthole/programming/common-lisp/cl-k-nearest-neighbors/Data\ Set/agaricus-lepiota.csv")