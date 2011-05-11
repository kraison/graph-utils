(in-package #:graph)

(defclass graph ()
  ((nodes     :accessor nodes     :initarg :nodes     :initform (make-hash-table :test 'equal))
   (ids       :accessor ids       :initarg :ids       :initform (make-hash-table))
   (last-id   :accessor last-id   :initarg :id        :initform -1)
   (edges     :accessor edges     :initarg :edges     :initform 0)
   (directed? :accessor directed? :initarg :directed? :initform nil)
   (matrix    :accessor matrix    :initarg :matrix    :initform (make-array '(0 0)
								     :adjustable t
								     :element-type 'integer
								     :initial-element 0))))

(defgeneric graph? (thing)
  (:method ((graph graph)) t)
  (:method (thing) nil))

(defmethod print-object ((graph graph) stream)
  (print-unreadable-object (graph stream :type t)
    (with-slots (ids directed?) graph
      (format stream "~A (~A vertices)" 
	      (if directed? "directed" "undirected") 
	      (hash-table-count ids)))))

(defun make-graph (&key directed?)
  (make-instance 'graph :directed? directed?))

(defmethod graph= ((g1 graph) (g2 graph))
  ;; FIXME: need to compare nodes and ids
  (and (equalp (matrix g1) (matrix g2))
       (= (last-id g1) (last-id g2))
       (eql (directed? g1) (directed? g2))
       (= (edges g1) (edges g2))))

(defmethod copy-graph ((graph graph))
  (let ((new-graph (make-instance 'graph
				  :matrix (make-array (list (array-dimension (matrix graph) 0)
							    (array-dimension (matrix graph) 1)))
				  :directed? (directed? graph)
				  :edges (edges graph)
				  :id (last-id graph))))
    (maphash #'(lambda (k v) (setf (gethash k (nodes new-graph)) v)) (nodes graph))
    (maphash #'(lambda (k v) (setf (gethash k (ids new-graph)) v)) (ids graph))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (loop for j from 0 to (1- (array-dimension (matrix graph) 1)) do
	      (setf (aref (matrix new-graph) i j) (aref (matrix graph) i j))))
    new-graph))

(defmethod undirected? ((graph graph))
  (null (directed? graph)))

(defmethod adjust-adjacency-matrix ((graph graph))
  "Grow the adjacency-matrix of the graph tp match the number of nodes."
  (adjust-array (matrix graph) (list (1+ (last-id graph)) (1+ (last-id graph))))
  #+allegro (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
		 (loop for j from 0 to (1- (array-dimension (matrix graph) 1)) do
		      (when (null (aref (matrix graph) i j))
			(setf (aref (matrix graph) i j) 0)))))

(defmethod add-node ((graph graph) value &key no-expand?)
  "Add a node to the graph.  If no-expand? it true, do not grow the adjacency-matrix."
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
	(unless no-expand?
	  (adjust-array (matrix graph) (list (1+ id) (1+ id))))
	(setf (gethash value (nodes graph)) id
	      (gethash id (ids graph)) value))))

(defmethod lookup-node ((graph graph) (value string))
  "Lookup a node's id"
  (gethash value (nodes graph)))

(defmethod lookup-node ((graph graph) (id integer))
  "Lookup a node's value"
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
      (reverse (if remove-nulls? (remove-if #'null r) r)))))

(defmethod list-nodes ((graph graph))
  "List all node values."
  (map-nodes #'(lambda (name id) (declare (ignore id)) name) graph :collect? t))

(defmethod node-ids ((graph graph))
  "List al lnode ids."
  (map-nodes #'(lambda (name id) (declare (ignore name)) id) graph :collect? t))

(defmethod node-count ((graph graph))
  "Return the node count."
  (hash-table-count (nodes graph)))

(defmethod neighbors ((graph graph) (node integer))
  "Return a list of ids for this node's neighbors."
  (let ((neighbors nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	 (when (> (aref (matrix graph) node i) 0)
	   (push i neighbors)))
    (when (directed? graph)
      (loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	   (when (> (aref (matrix graph) i node) 0)
	     (pushnew i neighbors))))
    (reverse neighbors)))

(defmethod neighbors ((graph graph) (node string))
  "Return a list of ids for this node's neighbors."
  (neighbors graph (gethash node (nodes graph))))

(defmethod inbound-edges ((graph graph) (node string))
  (inbound-edges graph (gethash node (nodes graph))))

(defmethod inbound-edges ((graph graph) (node integer))
  (if (directed? graph)
      (let ((neighbors nil))
	(loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	     (when (> (aref (matrix graph) i node) 0)
	       (pushnew i neighbors)))
	(nreverse neighbors))
      (error "inbound-edges does not makes sense in an undirected graph.")))

(defmethod outbound-edges ((graph graph) (node string))
  (outbound-edges graph (gethash node (nodes graph))))

(defmethod outbound-edges ((graph graph) (node integer))
  (if (directed? graph)
      (let ((neighbors nil))
	(loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	     (when (> (aref (matrix graph) node i) 0)
	       (pushnew i neighbors)))
	(nreverse neighbors))
      (error "inbound-edges does not makes sense in an undirected graph.")))

(defmethod edge-exists? ((graph graph) (n1 integer) (n2 integer))
  "Is there an edge between n1 and n2?"
  (when (> (aref (matrix graph) n1 n2) 0) (aref (matrix graph) n1 n2)))

(defmethod edge-exists? ((graph graph) (n1 string) (n2 string))
  "Is there an edge between n1 and n2?"
  (edge-exists? graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod add-edge ((graph graph) (n1 integer) (n2 integer) &key (weight 1))
  "Add an edge between n1 and n2."
  (unless (= n1 n2)
    (if (> (aref (matrix graph) n1 n2) 0)
	(format t "INFO: Already have an edge at ~A - ~A~%" n1 n2)
	(incf (edges graph)))
    (setf (aref (matrix graph) n1 n2) weight)    
    (when (undirected? graph)
      (setf (aref (matrix graph) n2 n1) weight))))

(defmethod add-edge ((graph graph) (n1 string) (n2 string) &key (weight 1))
  "Add an edge between n1 and n2."
  (add-edge graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph)) :weight weight))

(defmethod delete-edge ((graph graph) (n1 integer) (n2 integer))
  (unless (= n1 n2)
    (when (> (aref (matrix graph) n1 n2) 0)
      (decf (edges graph)))
    (setf (aref (matrix graph) n1 n2) 0)
    (when (undirected? graph)
      (setf (aref (matrix graph) n2 n1) 0))))

(defmethod delete-edge ((graph graph) (n1 string) (n2 string))
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
    (reverse (if remove-nulls? (remove-if #'null r) r))))

(defmethod list-edges ((graph graph))
  "Return all edges as pairs of nodes."
  (map-edges #'(lambda (n1 n2)
		 `(,(gethash n1 (ids graph)) ,(gethash n2 (ids graph))))
	     graph :collect? t :remove-nulls? t))

(defmethod edge-weight ((graph graph) (n1 integer) (n2 integer))
  (aref (matrix graph) n1 n2))

(defmethod edge-weight ((graph graph) (n1 string) (n2 string))
  (edge-weight graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod edge-count ((graph graph))
  "How many edges does the graph have?"
  (edges graph))

(defmethod slow-edge-count ((graph graph))
  (let ((count 0))
    (map-edges #'(lambda (n1 n2) (declare (ignore n1 n2)) (incf count)) graph)
    count))

