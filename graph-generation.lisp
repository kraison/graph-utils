(in-package #:graph-utils)

(defmethod generate-random-graph ((model (eql :meta)) (size integer)
				  &key degree (name-fn #'princ-to-string) 
				  &allow-other-keys)
  "Generate a random graph of SIZE nodes with average degree as close to DEGREE as possible."
  (let* ((graph (make-graph)) (queue nil))
    (dotimes (i size)
      (push i queue)
      (add-node graph (funcall name-fn i) :no-expand? t))
    (adjust-adjacency-matrix graph)
    (labels ((random-node ()
	       (let ((node (and (> (length queue) 0) (elt queue (random (length queue))))))
		 (when node
		   (if (>= (degree graph node) degree)
		       (progn
			 (setq queue (remove node queue))
			 (random-node))
		       node)))))
      (map-nodes #'(lambda (name id)
		     (declare (ignore name))
		     (let ((difference (- degree (degree graph id))))
		       (if (> difference 0)
			   (dotimes (i difference)
			     (let ((end-point (random-node)))
			       (when end-point
				 (add-edge graph id end-point))
			       (setq queue (remove id queue)))))))
		 graph)
      graph)))

(defmethod generate-random-graph ((model (eql :erdos-renyi)) (size integer) 
				  &key p (name-fn #'princ-to-string))
  (let ((graph (make-graph)))
    (dotimes (i size)
      (add-node graph (funcall name-fn i) :no-expand? t))
    (adjust-adjacency-matrix graph)
    (dotimes (i size)
      (loop for j from (1+ i) to (1- size) do
	   (when (<= (random 1.0) p)
	     (add-edge graph i j))))
    graph))

(defmethod generate-random-graph ((model (eql :barabasi-albert)) (size integer)
				  &key (saturation-point 0) (name-fn #'princ-to-string)
				  &allow-other-keys)
  (when (< size 4)
    (error "Cannot generate a barabasi-albert graph of size less than 4"))
  (let ((graph (make-graph :saturation-point saturation-point))
	(degree-table (make-array size :element-type 'integer :initial-element 0)))
    (dotimes (i 3)
      (add-node graph (funcall name-fn i)))
    (dotimes (i 3)
      (loop for j from (1+ i) to 2 do
	   (incf (aref degree-table i))
	   (incf (aref degree-table j))
	   (add-edge graph i j)))
    (loop for i from 3 to (1- size) do
	 (add-node graph (funcall name-fn i) :no-expand? t))
    (adjust-adjacency-matrix graph)
    (loop for i from 3 to (1- size) do
	 (loop for j from 0 to (1- i) do
	      (when (/= i j)
		(when (not (and (> (s-point graph) 0) 
				(>= (aref degree-table j) (s-point graph))))
		  (when (<= (random 1.0) 
			    ;; This is the traditional barabasi-albert calculation:
			    ;; (/ (aref degree-table j) (edge-count graph)))
			    ;; This is what we used for Lab 2:
			    (/ (1+ (aref degree-table j))
			       (+ (edge-count graph) (node-count graph))))
		      (incf (aref degree-table i))
		      (incf (aref degree-table j))
		      (add-edge graph i j))))))
    graph))

