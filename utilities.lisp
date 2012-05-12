(in-package #:graph-utils)

(defparameter *graph-utils-debug* t)

(defun dbg (control &rest args)
  "Debug output function"
  (when *graph-utils-debug*
    (apply #'format t control args)
    (terpri)))

