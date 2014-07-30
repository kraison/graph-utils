(in-package #:graph-utils)

(defmethod check-degree ((graph graph) degree)
  (map-nodes #'(lambda (name id)
		 (unless (= (degree graph id) degree)
		   (format t "~A: ~A has degree ~A~%"
			   id name (degree graph id))
		   id))
	     graph :collect? t :remove-nulls? t))

(defmethod generate-random-graph ((model (eql :viger-latapy)) (size integer)
				  &key degree (name-fn #'princ-to-string)
				  (swaps 20) (node-comparator 'equal)
                                  &allow-other-keys)
  "Generate a random, connected graph of SIZE nodes with average degree as
close to DEGREE as possible. Method based on
http://www-rp.lip6.fr/~latapy/Publis/random.pdf"
  (assert (and (integerp degree) (plusp degree)))
  (let* ((graph (make-graph :node-comparator node-comparator)) (queue nil))
    (dotimes (i size)
      (push i queue)
      (add-node graph (funcall name-fn i)))
    (labels ((choose-node ()
	       (setq queue (sort queue #'< :key #'(lambda (id)
						    (degree graph id))))
	       (let ((node (first queue)))
		 (when node
		   (if (>= (degree graph node) degree)
		       (progn
			 (pop queue)
			 (choose-node))
		       node)))))
      (map-nodes (lambda (name id)
                   (declare (ignore name))
                   (let ((difference (- degree (degree graph id))))
                     (if (> difference 0)
                         (dotimes (i difference)
                           (let ((end-point (choose-node)))
                             (when end-point
                               (add-edge graph id end-point))
                             (setq queue (remove id queue)))))))
		 graph)
      (unless (and (>= (edge-count graph) (1- (node-count graph)))
		   (null (zero-degree-nodes graph)))
	(error "Graph contains zero degree nodes!"))
      (let ((components (find-components graph)))
	(unless (= 1 (length components))
	  (error "Graph has more than one component!")))
      (dotimes (swap swaps)
	(let ((edge1 (random-edge graph)) (edge2 (random-edge graph)))
	  (swap-edges graph edge1 edge2)
	  ;;(unless (= 1 (length (find-components graph)))
	  ;;  (error "Edge swap of ~A and ~A disconnected the graph"
          ;;	   edge1 edge2))
          ))
	  (unless (= 1 (length (find-components graph)))
	    (error "Edge swaps disconnected the graph!"))
      graph)))

(defmethod generate-random-graph ((model (eql :erdos-renyi)) (size integer)
				  &key p (name-fn #'princ-to-string))
  (let ((graph (make-graph)))
    (dotimes (i size)
      (add-node graph (funcall name-fn i)))
    (dotimes (i size)
      (loop for j from (1+ i) to (1- size) do
	   (when (<= (random 1.0) p)
	     (add-edge graph i j))))
    graph))

(defmethod generate-random-graph ((model (eql :barabasi-albert))
				  (size integer)
				  &key (saturation-point 0)
				  (name-fn #'princ-to-string)
				  &allow-other-keys)
  (when (< size 4)
    (error "Cannot generate a barabasi-albert graph of size less than 4"))
  (let ((graph (make-graph :saturation-point saturation-point))
	(degree-table (make-array size
				  :element-type 'integer
				  :initial-element 0)))
    (dotimes (i 3)
      (add-node graph (funcall name-fn i)))
    (dotimes (i 3)
      (loop for j from (1+ i) to 2 do
	   (incf (aref degree-table i))
	   (incf (aref degree-table j))
	   (add-edge graph i j)))
    (loop for i from 3 to (1- size) do
	 (add-node graph (funcall name-fn i)))
    (loop for i from 3 to (1- size) do
	 (loop for j from 0 to (1- i) do
	      (when (/= i j)
		(when (not (and (> (s-point graph) 0)
				(>= (aref degree-table j) (s-point graph))))
		  (when (<= (random 1.0)
			    ;; This is the traditional barabasi-albert
			    ;; calculation:
			    ;; (/ (aref degree-table j) (edge-count graph)))
			    ;; This is what we used for Lab 2:
			    (/ (1+ (aref degree-table j))
			       (+ (edge-count graph) (node-count graph))))
		      (incf (aref degree-table i))
		      (incf (aref degree-table j))
		      (add-edge graph i j))))))
    graph))
