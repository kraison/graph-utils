(in-package #:graph-utils)

(defun print-node (node stream depth)
  (declare (ignore depth))
  (format stream "#<NODE ~A" (node-id node))
  (when (node-weight node)
    (format stream "(~A) " (node-weight node)))
  (format stream ": ~A>" (node-value node)))

(defstruct (node
             (:constructor %make-node)
	     (:print-function print-node))
  value
  id
  weight)

(let ((id 0))
  (defun next-node-id ()
    (incf id)))

(defun make-node (&key value id weight)
  (let ((id (or id (next-node-id))))
    (%make-node :value value :weight weight :id id)))

(defmethod node= (n1 n2)
  (= (node-id n1) (node-id n2)))

(defmethod node-eql (n1 n2)
  (and (= (node-id n1) (node-id n2))
       (eql (node-value n1) (node-value n2))))

(defmethod node-equal (n1 n2)
  (and (= (node-id n1) (node-id n2))
       (equal (node-value n1) (node-value n2))))

(defmethod node-equalp (n1 n2)
  (and (= (node-id n1) (node-id n2))
       (equalp (node-value n1) (node-value n2))))

