(in-package #:graph-utils)

(defun print-edge (edge stream depth)
  (declare (ignore depth))
  (format stream "#<EDGE ")
  (when (edge-weight edge)
    (format stream "(~A) " (edge-weight edge)))
  (format stream ": ~A,~A>" (edge-node1 edge) (edge-node2 edge)))

(defstruct (edge
	     (:print-function print-edge))
  node1
  node2
  weight)

;; Node / edge index
(defun sxhash-node (n)
  (sxhash (node-id n)))

(sb-ext:define-hash-table-test node= sxhash-node)

(defun make-node-table ()
  (make-hash-table :test 'node=))

(defstruct (edge-index (:conc-name nil)
                       (:constructor %make-edge-index))
  table)

(defun make-edge-index ()
  (%make-edge-index :table (make-node-table)))

(defmethod add-edge-to-index ((index edge-index) edge)
  (let ((table (or (gethash (edge-node1 edge) (table index))
                   (setf (gethash (edge-node1 edge) (table index))
                         (make-node-table)))))
    (setf (gethash (edge-node2 edge) table) edge)))

(defun remove-edge-from-index (index edge)
  (let ((table (or (gethash (edge-node1 edge) (table index))
                   (setf (gethash (edge-node1 edge) (table index))
                         (make-node-table )))))
    (remhash (edge-node2 edge) table)))

(defun lookup-edge (index node1 node2)
  (let ((table (gethash node1 (table index))))
    (when (hash-table-p table)
      (gethash node2 table))))

