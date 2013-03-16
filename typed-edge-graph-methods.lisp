(in-package #:graph-utils)

(declaim (optimize (speed 3) (space 2)))

(defmethod in-degree ((graph typed-graph) (node integer) &key edge-type)
  (let ((degree 0) (matrix (gethash edge-type (matrix graph))))
    (map-sarray-col #'(lambda (i w)
                        (when (> w 0)
                          (incf degree)))
                    matrix node)
    degree))

(defmethod in-degree ((graph typed-graph) node &key edge-type)
  (in-degree graph (lookup-node graph node) :edge-type edge-type))

(defmethod out-degree ((graph typed-graph) (node integer) &key edge-type)
  (let ((degree 0) (matrix (gethash edge-type (matrix graph))))
    (map-sarray-row #'(lambda (i w)
                        (when (> w 0)
                          (incf degree)))
                    matrix node)
	degree))

(defmethod out-degree ((graph typed-graph) node &key edge-type)
  (out-degree graph (lookup-node graph node) :edge-type edge-type))

(defmethod degree-distribution ((graph typed-graph) &key edge-type)
  (let ((dist nil) (matrix (gethash edge-type (matrix graph))))
    (maphash #'(lambda (node id)
		 (declare (ignore node))
		 (let ((degree 0))
		   (loop
                      for i
                      from 0
                      to (1- (row-count matrix)) do
			(when (not (zerop (saref matrix id i)))
			  (incf degree)))
		   (if (assoc degree dist)
		       (incf (cdr (assoc degree dist)))
		       (push (cons degree 1) dist))))
	     (nodes graph))
    (sort dist #'< :key 'car)))

(defmethod in-degree-distribution ((graph typed-graph) &key edge-type)
  (let ((dist nil) (matrix (gethash edge-type (matrix graph))))
    (maphash #'(lambda (node id)
		 (declare (ignore node))
		 (let ((degree 0))
		   (loop
                      for i
                      from 0
                      to (1- (col-count matrix)) do
			(when (not (zerop (saref matrix i id)))
			  (incf degree)))
		   (if (assoc degree dist)
		       (incf (cdr (assoc degree dist)))
		       (push (cons degree 1) dist))))
	     (nodes graph))
    (sort dist #'< :key 'car)))

(defmethod sim-rank ((graph typed-graph) n1 n2 &key edge-type)
  )