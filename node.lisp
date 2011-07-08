(in-package #:graph-utils)

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<NODE ~A: ~A>" (node-id node) (node-value node)))

(defstruct (node
	     (:print-function print-node))
  value
  id)

(defmethod node-equal (n1 n2)
  (and (= (node-id n1) (node-id n2))
       (equal (node-value n1) (node-value n2))))

(defmethod node-equalp (n1 n2)
  (and (= (node-id n1) (node-id n2))
       (equalp (node-value n1) (node-value n2))))

