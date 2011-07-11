(in-package #:graph-utils)

(declaim (optimize (speed 3) (space 2)))

(defclass graph ()
  ((nodes      :accessor nodes      :initarg :nodes      :initform (make-hash-table :test 'equal))
   (ids        :accessor ids        :initarg :ids        :initform (make-hash-table))
   (s-point    :accessor s-point    :initarg :s-point    :initform 0)
   (last-id    :accessor last-id    :initarg :id         :initform -1)
   (edges      :accessor edges      :initarg :edges      :initform 0)
   (comparator :accessor comparator :initarg :comparator :initform 'equal)
   (degree-table :accessor degree-table :initarg :degree-table :initform (make-hash-table))
   (matrix     :accessor matrix     :initarg :matrix     :initform (make-array '(0 0)
								     :adjustable t
								     :element-type 'number
								     :initial-element 0))))

(defgeneric graph? (thing)
  (:documentation "graph predicate")
  (:method ((thing graph)) t)
  (:method (thing) nil))

(defclass directed-graph (graph)
  ((in-degree-table  :accessor in-degree-table  :initarg :in-degree-table  :initform (make-hash-table))
   (out-degree-table :accessor out-degree-table :initarg :out-degree-table :initform (make-hash-table))))

(defgeneric directed? (thing)
  (:documentation "directed graph predicate")
  (:method ((thing directed-graph)) t)
  (:method (thing) nil))

(defmethod print-object ((graph graph) stream)
  "Print a graph"
  (print-unreadable-object (graph stream :type t)
    (format stream "~A (~A vertices & ~A edges)"
	    (if (directed? graph) "directed" "undirected") 
	    (hash-table-count (ids graph)) (edge-count graph))))

(defun make-graph (&key directed? (node-comparator #'equal) (saturation-point 0))
  "Create a new graph object"
  (make-instance (if directed? 'directed-graph 'graph)
		 :comparator node-comparator
		 :s-point saturation-point
		 :nodes (make-hash-table :test node-comparator)))

(defmethod graph-equal ((g1 graph) (g2 graph))
  "In-depth graph equality check."
  (and (= (last-id g1) (last-id g2))
       (eql (comparator g1) (comparator g2))
       (eql (directed? g1) (directed? g2))
       (= (s-point g1) (s-point g2))
       (= (edges g1) (edges g2))
       (= (hash-table-count (nodes g1)) (hash-table-count (nodes g2)))
       (= (hash-table-count (ids g1)) (hash-table-count (ids g2)))
       (progn
	 (maphash #'(lambda (k v1)
		      (let ((v2 (gethash k (nodes g2))))
			(unless (and (integerp v2) (= v1 v2))
			  (return-from graph-equal nil))))
		  (nodes g1))
	 (maphash #'(lambda (k v1)
		      (let ((v2 (gethash k (ids g2))))
			(unless (funcall (comparator g1) v1 v2)
			  (return-from graph-equal nil))))
		  (ids g1))
	 (equalp (matrix g1) (matrix g2)))))

(defmethod copy-graph ((graph graph))
  "Make a deep copy of a graph."
  (let ((new-graph (make-instance (if (directed? graph) 'directed-graph 'graph)
				  :matrix (make-array (list (array-dimension (matrix graph) 0)
							    (array-dimension (matrix graph) 1)))
				  :edges (edges graph)
				  :id (last-id graph))))
    (maphash #'(lambda (k v) (setf (gethash k (nodes new-graph)) v)) (nodes graph))
    (maphash #'(lambda (k v) (setf (gethash k (ids new-graph)) v)) (ids graph))
    (maphash #'(lambda (k v) (setf (gethash k (degree-table new-graph)) v)) (degree-table graph))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (loop for j from 0 to (1- (array-dimension (matrix graph) 1)) do
	      (setf (aref (matrix new-graph) i j) (aref (matrix graph) i j))))
    new-graph))

(defmethod undirected? ((graph graph))
  (null (directed? graph)))

(defmethod adjust-adjacency-matrix ((graph graph))
  "Grow the adjacency-matrix of the graph to match the number of nodes."
  (adjust-array (matrix graph) (list (1+ (last-id graph)) 
				     (1+ (last-id graph))))
  #+allegro (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) 
	       do
	       (loop for j from 0 to (1- (array-dimension (matrix graph) 1)) 
		  do
		  (when (null (aref (matrix graph) i j))
		    (setf (aref (matrix graph) i j) 0)))))

(defmethod add-node ((graph graph) value &key no-expand?)
  "Add a node to the graph.  If no-expand? it true, do not grow the adjacency-matrix. It is
recommended that when adding nodes in bulk, you use no-expand? and call adjust-adjacency-matrix
after all nodes have been added."
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
	(unless no-expand?
	  (adjust-array (matrix graph) (list (1+ id) (1+ id))))
	(when (directed? graph)
	  (setf (gethash id (in-degree-table graph)) 0
		(gethash id (out-degree-table graph)) 0))
	(setf (gethash id (degree-table graph)) 0
	      (gethash value (nodes graph)) id
	      (gethash id (ids graph)) value)
	id)))

(defmethod rename-node ((graph graph) (id integer) name)
  (let ((old-name (lookup-node graph id)))
    (remhash old-name (nodes graph))
    (setf (gethash name (nodes graph)) id
	  (gethash id (ids graph)) name)))

(defmethod lookup-node ((graph graph) value)
  "Lookup a node based on value."
  (gethash value (nodes graph)))

(defmethod lookup-node ((graph graph) (id integer))
  "Lookup a node based on id"
  (gethash id (ids graph)))

(defmethod map-nodes ((fn function) (graph graph) &key collect? remove-nulls?)
  "Apply a function to all nodes."
  (let ((r nil))
    (maphash #'(lambda (node-name node-id)
		 (if collect?
		     (push (funcall fn node-name node-id) r)
		     (funcall fn node-name node-id)))
	     (nodes graph))
    (when collect?
      (nreverse (if remove-nulls? (remove-if #'null r) r)))))

(defmethod list-nodes ((graph graph))
  "List all node values."
  (map-nodes #'(lambda (name id) (declare (ignore id)) name) graph :collect? t))

(defmethod node-ids ((graph graph))
  "List al lnode ids."
  (map-nodes #'(lambda (name id) (declare (ignore name)) id) graph :collect? t))

(defmethod node-count ((graph graph))
  "Return the node count."
  (hash-table-count (nodes graph)))

(defmethod neighbors ((graph graph) (node integer) &key (return-ids? t))
  "Return a list of ids for this node's neighbors. Returns inbound and outbound 
neighbors for a directed graph."
  (let ((neighbors nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	 (when (> (aref (matrix graph) node i) 0)
	   (push i neighbors)))
    (when (directed? graph)
      (loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	   (when (> (aref (matrix graph) i node) 0)
	     (pushnew i neighbors))))
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'lookup-node (nreverse neighbors)))))

(defmethod neighbors ((graph graph) node &key (return-ids? t))
  "Return a list of ids for this node's neighbors."
  (neighbors graph (gethash node (nodes graph)) :return-ids? return-ids?))

(defmethod inbound-neighbors ((graph directed-graph) node &key (return-ids? t))
  (inbound-neighbors graph (gethash node (nodes graph)) :return-ids? return-ids?))

(defmethod inbound-neighbors ((graph directed-graph) (node integer) &key (return-ids? t))
  (let ((neighbors nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	 (when (> (aref (matrix graph) i node) 0)
	   (pushnew i neighbors)))
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'lookup-node (nreverse neighbors)))))

(defmethod outbound-neighbors ((graph directed-graph) node &key (return-ids? t))
  (outbound-neighbors graph (gethash node (nodes graph)) :return-ids? return-ids?))

(defmethod outbound-neighbors ((graph directed-graph) (node integer) &key (return-ids? t))
  (let ((neighbors nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (when (> (aref (matrix graph) node i) 0)
	   (pushnew i neighbors)))
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'lookup-node (nreverse neighbors)))))

(defmethod edge-exists? ((graph graph) (n1 integer) (n2 integer))
  "Is there an edge between n1 and n2?"
  (when (> (aref (matrix graph) n1 n2) 0) (aref (matrix graph) n1 n2)))

(defmethod edge-exists? ((graph graph) n1 n2)
  "Is there an edge between n1 and n2?"
  (edge-exists? graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod add-edge ((graph graph) (n1 integer) (n2 integer) &key (weight 1))
  "Add an edge between n1 and n2."
  (unless (= n1 n2)
    (unless (> (aref (matrix graph) n1 n2) 0)
      (incf (gethash n1 (degree-table graph)))
      (incf (gethash n2 (degree-table graph)))
      (incf (edges graph)))
    (setf (aref (matrix graph) n1 n2) weight)
    (setf (aref (matrix graph) n2 n1) weight))
  (list n1 n2))

(defmethod add-edge ((graph graph) n1 n2 &key (weight 1))
  "Add an edge between n1 and n2."
  (add-edge graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph)) :weight weight))

(defmethod add-edge ((graph directed-graph) (n1 integer) (n2 integer) &key (weight 1))
  (unless (= n1 n2)
    (unless (> (aref (matrix graph) n1 n2) 0)
      (incf (gethash n1 (out-degree-table graph)))
      (incf (gethash n2 (in-degree-table graph)))
      (incf (edges graph)))
    (setf (aref (matrix graph) n1 n2) weight)
    (list n1 n2)))

(defmethod add-edge ((graph directed-graph) n1 n2 &key (weight 1))
  "Add an edge between n1 and n2."
  (add-edge graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph)) :weight weight))

(defmethod delete-edge ((graph graph) (n1 integer) (n2 integer))
  "Remove an edge from the graph."
  (unless (= n1 n2)
    (when (> (aref (matrix graph) n1 n2) 0)
      (decf (gethash n1 (degree-table graph)))
      (decf (gethash n2 (degree-table graph)))
      (decf (edges graph))
      (setf (aref (matrix graph) n1 n2) 0))
    (setf (aref (matrix graph) n2 n1) 0)))

(defmethod delete-edge ((graph directed-graph) (n1 integer) (n2 integer))
  "Remove an edge from the graph."
  (unless (= n1 n2)
    (when (> (aref (matrix graph) n1 n2) 0)
      (decf (gethash n1 (out-degree-table graph)))
      (decf (gethash n2 (in-degree-table graph)))
      (decf (edges graph))
      (setf (aref (matrix graph) n1 n2) 0))))

(defmethod delete-edge ((graph graph) n1 n2)
  (delete-edge graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod map-edges ((fn function) (graph graph) &key collect? remove-nulls?)
  "Apply a function to all edges."
  (let ((r nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (loop for j from (if (directed? graph) 0 i) to (1- (array-dimension (matrix graph) 1)) do
	      (when (> (aref (matrix graph) i j) 0)
		(if collect?
		    (push (funcall fn i j) r)
		    (funcall fn i j)))))
    (nreverse (if remove-nulls? (remove-if #'null r) r))))

(defmethod list-edges ((graph graph))
  "Return all edges as pairs of nodes."
  (map-edges #'(lambda (n1 n2)
		 `(,(gethash n1 (ids graph)) ,(gethash n2 (ids graph))))
	     graph :collect? t :remove-nulls? t))

(defmethod edge-weight ((graph graph) (n1 integer) (n2 integer))
  (aref (matrix graph) n1 n2))

(defmethod edge-weight ((graph graph) n1 n2)
  (edge-weight graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod edge-count ((graph graph))
  "How many edges does the graph have?"
  (edges graph))

(defmethod slow-edge-count ((graph graph))
  (let ((count 0))
    (map-edges #'(lambda (n1 n2) (declare (ignore n1 n2)) (incf count)) graph)
    count))

(defmethod random-edge ((graph graph))
  (let (n1 n2 (w 0))
    (loop until (> w 0) do
	 (setq n1 (random (array-dimension (matrix graph) 0))
	       n2 (random (array-dimension (matrix graph) 1)))
	 (setq w (aref (matrix graph) n1 n2)))
    (list n1 n2)))

(defmethod swap-edges ((graph graph) e1 e2)
  (apply #'delete-edge (cons graph e1))
  (apply #'delete-edge (cons graph e2))
  (add-edge graph (first e1) (first e2))
  (add-edge graph (second e1) (second e2)))

(defmethod degree ((graph graph) node)
  (degree graph (gethash node (nodes graph))))

(defmethod degree ((graph graph) (node integer))
  "Calculate the degree of a node."
  (if (undirected? graph)
      (gethash node (degree-table graph))
      (error "Cannot calculate the degree in a directed graph.  Use in-degree or out-degree instead.")))

(defmethod leaves ((graph directed-graph))
  (map-nodes #'(lambda (name id)
		 (declare (ignore name))
		 (when (= (out-degree graph id) 0)
		   id))
	     graph :collect? t :remove-nulls? t))
