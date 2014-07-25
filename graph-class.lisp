(in-package #:graph-utils)

(declaim (optimize (speed 3) (space 2)))

(defclass graph ()
  ((nodes        :accessor nodes      :initarg :nodes
                 :initform (make-hash-table :test 'equal))
   (ids          :accessor ids        :initarg :ids
                 :initform (make-hash-table))
   (node-caps    :accessor node-caps  :initarg :node-caps
                 :initform (make-hash-table))
   (s-point      :accessor s-point    :initarg :s-point    :initform 0)
   (last-id      :accessor last-id    :initarg :id         :initform -1)
   (edges        :accessor edges      :initarg :edges      :initform 0)
   (comparator   :accessor comparator :initarg :comparator :initform 'equal)
   (degree-table :accessor degree-table :initarg :degree-table
                 :initform (make-hash-table))
   (matrix       :accessor matrix     :initarg :matrix
                 :initform (make-sparse-array '(0 0)
                                              :adjustable t
                                              :element-type 'number
                                              :initial-element 0))))

(defgeneric graph? (thing)
  (:documentation "graph predicate")
  (:method ((thing graph)) t)
  (:method (thing) nil))

(defclass directed-graph (graph)
  ((in-degree-table  :accessor in-degree-table  :initarg :in-degree-table
                     :initform (make-hash-table))
   (out-degree-table :accessor out-degree-table :initarg :out-degree-table
                     :initform (make-hash-table))))

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

(defun make-graph (&key directed? (node-comparator #'equal)
                   (saturation-point 0))
  "Create a new graph object. You are responsible for making sure that
node-comparator is a valid hash table test."
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
         ;; FIXME: compare edge-index
	 (equalp (matrix g1) (matrix g2)))))

(defmethod copy-graph ((graph graph))
  "Make a deep copy of a graph."
  (let ((new-graph (make-instance
                    (if (directed? graph) 'directed-graph 'graph)
                    :matrix (make-sparse-array
                             (list (row-count (matrix graph))
                                   (col-count (matrix graph)))
                             :adjustable t
                             :element-type 'number
                             :initial-element 0)
                    :edges (edges graph)
                    :id (last-id graph))))
    (maphash #'(lambda (k v) (setf (gethash k (nodes new-graph)) v))
             (nodes graph))
    (maphash #'(lambda (k v) (setf (gethash k (ids new-graph)) v))
             (ids graph))
    (maphash #'(lambda (k v) (setf (gethash k (node-caps new-graph)) v))
             (node-caps graph))
    (when (directed? graph)
      (maphash #'(lambda (k v)
                   (setf (gethash k (in-degree-table new-graph)) v))
               (in-degree-table graph))
      (maphash #'(lambda (k v)
                   (setf (gethash k (out-degree-table new-graph)) v))
               (out-degree-table graph)))
    (maphash #'(lambda (k v) (setf (gethash k (degree-table new-graph)) v))
             (degree-table graph))
    (fast-map-sarray #'(lambda (i j w)
                         (setf (saref (matrix new-graph) i j) w))
                     (matrix graph))
    new-graph))

(defmethod undirected? ((graph graph))
  (null (directed? graph)))

(defmethod adjust-adjacency-matrix ((graph graph))
  "Grow the adjacency-matrix of the graph to match the number of nodes."
  nil)

(defmethod add-node ((graph graph) value &key capacity)
  "Add a node to the graph."
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
        (incf-sarray-dimensions (matrix graph))
        (when capacity
          (setf (gethash id (node-caps graph)) capacity))
	(when (directed? graph)
	  (setf (gethash id (in-degree-table graph)) 0
		(gethash id (out-degree-table graph)) 0))
	(setf (gethash id (degree-table graph)) 0
	      (gethash value (nodes graph)) id
	      (gethash id (ids graph)) value)
	id)))

(defmethod set-capacity ((graph graph) (node-id integer) capacity)
  (setf (gethash node-id (node-caps graph)) capacity))

(defmethod set-capacity ((graph graph) node capacity)
  (set-capacity graph (lookup-node graph node) capacity))

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
    (maphash (lambda (node-name node-id)
               (if collect?
                   (push (funcall fn node-name node-id) r)
                  (funcall fn node-name node-id)))
	     (nodes graph))
    (when collect?
      (nreverse (if remove-nulls? (remove-if #'null r) r)))))

(defmethod list-nodes ((graph graph))
  "List all node values."
  (map-nodes (lambda (name id)
               (declare (ignore id))
               name)
             graph :collect? t))

(defmethod node-ids ((graph graph))
  "List all node ids."
  (map-nodes (lambda (name id)
               (declare (ignore name))
               id)
             graph :collect? t))

(defmethod random-node-id ((graph graph))
  (let ((counter 0) (rand (random (node-count graph))))
    (map-nodes (lambda (name id)
                 (declare (ignore name))
                 (when (= counter rand)
                   (return-from random-node-id id))
                 (incf counter))
               graph :collect? nil)))

(defmethod random-node ((graph graph))
  (let ((counter 0) (rand (random (node-count graph))))
    (map-nodes (lambda (name id)
                 (declare (ignore id))
                 (when (= counter rand)
                   (return-from random-node name))
                 (incf counter))
               graph :collect? nil)))

(defmethod node-count ((graph graph))
  "Return the node count."
  (hash-table-count (nodes graph)))

(defgeneric neighbors (graph node &key return-ids? edge-type))
(defmethod neighbors ((graph graph) (node integer) &key (return-ids? t)
                      &allow-other-keys)
  "Return a list of ids for this node's neighbors. Returns inbound and
outbound neighbors for a directed graph."
  (let ((neighbors nil))
    (map-sarray-col #'(lambda (row-id value)
                        (when (> value 0)
                          (push row-id neighbors)))
                    (matrix graph) node)
    (when (directed? graph)
      (map-sarray-row #'(lambda (col-id value)
                          (when (> value 0)
                            (push col-id neighbors)))
                      (matrix graph) node))
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'(lambda (id)
                    (lookup-node graph id))
                (nreverse neighbors)))))

(defmethod neighbors ((graph graph) node &key (return-ids? t) &allow-other-keys)
  "Return a list of ids for this node's neighbors."
  (neighbors graph (gethash node (nodes graph)) :return-ids? return-ids?))

(defgeneric inbound-neighbors (graph node &key return-ids? edge-type))
(defmethod inbound-neighbors ((graph directed-graph) node &key
                              (return-ids? t) &allow-other-keys)
  (inbound-neighbors graph
                     (gethash node (nodes graph))
                     :return-ids? return-ids?))

(defmethod inbound-neighbors ((graph directed-graph) (node integer) &key
                              (return-ids? t) &allow-other-keys)
  (let ((neighbors nil))
    (map-sarray-col #'(lambda (row-id value)
                        (when (> value 0)
                          (push row-id neighbors)))
                    (matrix graph) node)
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'(lambda (id)
                    (lookup-node graph id))
                (nreverse neighbors)))))

(defgeneric outbound-neighbors (graph node &key return-ids? edge-type))
(defmethod outbound-neighbors ((graph directed-graph) node &key
                               (return-ids? t) &allow-other-keys)
  (outbound-neighbors graph
                      (gethash node (nodes graph))
                      :return-ids? return-ids?))

(defmethod outbound-neighbors ((graph directed-graph) (node integer) &key
                               (return-ids? t) &allow-other-keys)
  (let ((neighbors nil))
    (map-sarray-row #'(lambda (col-id value)
                        (when (> value 0)
                          (push col-id neighbors)))
                    (matrix graph) node)
    (if return-ids?
	(nreverse neighbors)
	(mapcar #'(lambda (id)
                    (lookup-node graph id))
                (nreverse neighbors)))))

(defgeneric edge-exists? (graph n1 n2 &key edge-type))
(defmethod edge-exists? ((graph graph) (n1 integer) (n2 integer)
                         &key &allow-other-keys)
  "Is there an edge between n1 and n2?"
  (when (> (saref (matrix graph) n1 n2) 0) (saref (matrix graph) n1 n2)))

(defmethod edge-exists? ((graph graph) n1 n2 &key &allow-other-keys)
  "Is there an edge between n1 and n2?"
  (edge-exists? graph (lookup-node graph n1) (lookup-node graph n2)))

(defgeneric add-edge (graph n1 n2 &key weight edge-type))
(defmethod add-edge ((graph graph) (n1 integer) (n2 integer) &key (weight 1)
                     &allow-other-keys)
  "Add an edge between n1 and n2."
  (unless (= n1 n2)
    (unless (> (saref (matrix graph) n1 n2) 0)
      (incf (gethash n1 (degree-table graph)))
      (incf (gethash n2 (degree-table graph)))
      (incf (edges graph)))
    (setf (saref (matrix graph) n1 n2) weight)
    (setf (saref (matrix graph) n2 n1) weight))
  (list n1 n2))

(defmethod add-edge ((graph directed-graph) (n1 integer) (n2 integer) &key
                     (weight 1) &allow-other-keys)
  (unless (= n1 n2)
    (unless (> (saref (matrix graph) n1 n2) 0)
      (incf (gethash n1 (out-degree-table graph)))
      (incf (gethash n2 (in-degree-table graph)))
      (incf (edges graph)))
    (setf (saref (matrix graph) n1 n2) weight)
    (list n1 n2)))

(defmethod add-edge ((graph graph) n1 n2 &key (weight 1) &allow-other-keys)
  "Add an edge between n1 and n2."
  (add-edge graph
            (gethash n1 (nodes graph))
            (gethash n2 (nodes graph))
            :weight weight))

(defmethod add-edge ((graph directed-graph) n1 n2 &key (weight 1)
                     &allow-other-keys)
  "Add an edge between n1 and n2."
  (add-edge graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))
            :weight weight))

(defgeneric delete-edge (graph n1 n2 &optional edge-type))
(defmethod delete-edge ((graph graph) (n1 integer) (n2 integer) &optional et)
  "Remove an edge from the graph."
  (declare (ignore et))
  (unless (= n1 n2)
    (when (> (saref (matrix graph) n1 n2) 0)
      (decf (gethash n1 (degree-table graph)))
      (decf (gethash n2 (degree-table graph)))
      (decf (edges graph))
      (setf (saref (matrix graph) n1 n2) 0))
    (setf (saref (matrix graph) n2 n1) 0)))

(defmethod delete-edge ((graph directed-graph) (n1 integer) (n2 integer)
                        &optional et)
  "Remove an edge from the graph."
  (declare (ignore et))
  (unless (= n1 n2)
    ;;(dbg "Deleting edge (~A,~A)" n1 n2)
    (when (> (saref (matrix graph) n1 n2) 0)
      ;;(dbg "Decrementing out-degree table entry for ~A (was ~A)"
      ;;n1 (gethash n1 (out-degree-table graph)))
      (decf (gethash n1 (out-degree-table graph)))
      ;;(dbg "Decrementing in-degree table entry for ~A (was ~A)"
      ;;n2 (gethash n2 (in-degree-table graph)))
      (decf (gethash n2 (in-degree-table graph)))
      (decf (edges graph))
      (setf (saref (matrix graph) n1 n2) 0))))

(defmethod delete-edge ((graph graph) n1 n2 &optional et)
  (declare (ignore et))
  (delete-edge graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod map-edges ((fn function) (graph graph) &key collect? remove-nulls?)
  "Apply a function to all edges."
  (let ((r nil))
    (fast-map-sarray #'(lambda (n1 n2 w)
                         (let ((v (funcall fn n1 n2 w)))
                           (when collect?
                             (push (list n1 n2 v) r))))
                     (matrix graph))
    (nreverse (if remove-nulls?
                  (remove-if #'(lambda (triple)
                                 (null (third triple)))
                             r)
                  r))))

(defmethod list-edges ((graph graph) &key nodes-as-ids)
  "Return all edges as pairs of nodes."
  (let ((r nil))
    (fast-map-sarray #'(lambda (n1 n2 w)
                         (declare (ignore w))
                         (push (if nodes-as-ids
                                   (list n1 n2)
                                   (list (gethash n1 (ids graph))
                                         (gethash n2 (ids graph))))
                               r))
                     (matrix graph))
    (nreverse r)))

(defgeneric set-edge-weight (graph n1 n2 weight &key edge-type))
(defmethod set-edge-weight ((graph graph) (n1 integer) (n2 integer) weight
                            &key &allow-other-keys)
  (setf (saref (matrix graph) n1 n2) weight))

(defgeneric edge-weight (graph n1 n2 &optional edge-type))
(defmethod edge-weight ((graph graph) (n1 integer) (n2 integer) &optional et)
  (declare (ignore et))
  (saref (matrix graph) n1 n2))

(defmethod edge-weight ((graph graph) n1 n2 &optional et)
  (declare (ignore et))
  (edge-weight graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defgeneric incf-edge-weight (graph n1 n2 &key edge-type delta))
(defmethod incf-edge-weight ((graph graph) (n1 integer) (n2 integer)
                             &key (delta 1) &allow-other-keys)
  (incf-sarray (matrix graph) (list n1 n2) delta))

(defmethod incf-edge-weight ((graph graph) n1 n2 &key delta &allow-other-keys)
  (incf-edge-weight graph
                    (lookup-node graph n1)
                    (lookup-node graph n2)
                    :delta delta))

(defgeneric decf-edge-weight (graph n1 n2 &key edge-type delta))
(defmethod decf-edge-weight ((graph graph) (n1 integer) (n2 integer)
                             &key (delta 1) &allow-other-keys)
  (decf-sarray (matrix graph) (list n1 n2) delta))

(defmethod decf-edge-weight ((graph graph) n1 n2 &key delta &allow-other-keys)
  (decf-edge-weight graph
                    (gethash n1 (nodes graph))
                    (gethash n2 (nodes graph))
                    :delta delta))

(defmethod capacity ((graph graph) n1 n2)
  (or (edge-weight graph n1 n2) 0))

(defmethod node-capacity ((graph graph) node)
  (min
   (apply #'+ (mapcar #'(lambda (n2)
                          (capacity graph node n2))
                      (outbound-neighbors graph node)))
   (apply #'+ (mapcar #'(lambda (n2)
                          (capacity graph n2 node))
                      (inbound-neighbors graph node)))))

(defmethod minimum-capacity ((graph graph) edges)
  (let ((min most-positive-fixnum) (min-list nil))
    (dolist (edge edges)
      (when (< (apply #'edge-weight graph edge) min)
        (setq min (apply #'edge-weight graph edge))
        (push edge min-list)))
    (values min min-list)))

(defmethod edge-count ((graph graph))
  "How many edges does the graph have?"
  (edges graph))

(defmethod slow-edge-count ((graph graph))
  (let ((count 0))
    (map-edges #'(lambda (n1 n2 w) (declare (ignore n1 n2 w)) (incf count)) graph)
    count))

(defgeneric random-edge (graph &optional edge-type))
(defmethod random-edge ((graph graph) &optional et)
  (declare (ignore et))
  (let (n1 n2 (w 0))
    (loop until (> w 0) do
	 (setq n1 (random (row-count (matrix graph)))
	       n2 (random (col-count (matrix graph)))
               w (saref (matrix graph) n1 n2)))
    (list n1 n2)))

(defmethod swap-edges ((graph graph) e1 e2)
  (apply #'delete-edge (cons graph e1))
  (apply #'delete-edge (cons graph e2))
  (add-edge graph (first e1) (first e2))
  (add-edge graph (second e1) (second e2)))

(defgeneric reverse-edge (graph n1 n2 &optional edge-type))
(defmethod reverse-edge ((graph graph) n1 n2 &optional edge-type)
  (declare (ignore edge-type))
  (let ((weight (edge-weight graph n1 n2)))
    (delete-edge graph n1 n2)
    (add-edge graph n2 n1 :weight weight)))

(defmethod reverse-all-edges ((graph graph))
  (dolist (edge (list-edges graph :nodes-as-ids t))
    (let ((weight (edge-weight graph (first edge) (second edge))))
      (delete-edge graph (first edge) (second edge))
      (add-edge graph (second edge) (first edge) :weight weight)))
  graph)

(defmethod degree ((graph graph) node)
  (degree graph (gethash node (nodes graph))))

(defmethod degree ((graph graph) (node integer))
  "Calculate the degree of a node."
  (if (undirected? graph)
      (gethash node (degree-table graph))
      (error "Cannot calculate the degree in a directed graph.  ~
              Use in-degree or out-degree instead.")))

(defmethod leaf? ((graph graph) (id integer))
  (= (out-degree graph id) 0))

(defmethod leaf? ((graph graph) node)
  (leaf? graph (lookup-node graph node)))

(defmethod leaves ((graph directed-graph))
  (map-nodes #'(lambda (name id)
		 (declare (ignore name))
		 (when (leaf? graph id)
		   id))
	     graph :collect? t :remove-nulls? t))

