(in-package #:graph-utils)

(declaim (optimize (speed 3) (space 2)))

(defmethod density ((graph graph))
  "Calculate the graph's density."
  (coerce
   (/ (edge-count graph)
      (/ (* (node-count graph) (- (node-count graph) 1)) 2))
   'float))

(defmethod zero-degree-nodes ((graph graph))
  (map-nodes #'(lambda (name id)
		 (declare (ignore name))
		 (when (= (degree graph id) 0)
		   id))
	     graph :collect? t :remove-nulls? t))

(defgeneric in-degree (graph node &key edge-type))
(defmethod in-degree ((graph graph) (node integer) &key &allow-other-keys)
  (if (directed? graph)
      (let ((degree 0))
        (map-sarray-col #'(lambda (i w)
                            (when (> w 0)
                              (incf degree)))
                        (matrix graph) node)
	degree)
      (error "Cannot calculate in-degree on an undirected graph")))

(defgeneric out-degree (graph node &key edge-type))
(defmethod out-degree ((graph graph) (node integer) &key &allow-other-keys)
  (if (directed? graph)
      (let ((degree 0))
        (map-sarray-row #'(lambda (i w)
                            (when (> w 0)
                              (incf degree)))
                        (matrix graph) node)
	degree)
      (error "Cannot calculate out-degree on an undirected graph")))

(defgeneric degree-distribution (graph &key edge-type))
(defmethod degree-distribution ((graph graph) &key &allow-other-keys)
  "Generate the degree distribution for the graph. For a directed graph,
returns the out-degree distribution."
  (let ((dist nil))
    (maphash #'(lambda (node id)
		 (declare (ignore node))
		 (let ((degree 0))
		   (loop
                      for i
                      from 0
                      to (1- (row-count (matrix graph))) do
			(when (not (zerop (saref (matrix graph) id i)))
			  (incf degree)))
		   (if (assoc degree dist)
		       (incf (cdr (assoc degree dist)))
		       (push (cons degree 1) dist))))
	     (nodes graph))
    (sort dist #'< :key 'car)))

(defgeneric in-degree-distribution (graph &key edge-type))
(defmethod in-degree-distribution ((graph graph) &key &allow-other-keys)
  "Generate the degree distribution for the graph. For a directed graph,
returns the out-degree distribution."
  (unless (directed? graph)
    (error "Cannot compute in-degree-distribution on an undirected graph"))
  (let ((dist nil))
    (maphash #'(lambda (node id)
		 (declare (ignore node))
		 (let ((degree 0))
		   (loop
                      for i
                      from 0
                      to (1- (col-count (matrix graph))) do
			(when (not (zerop (saref (matrix graph) i id)))
			  (incf degree)))
		   (if (assoc degree dist)
		       (incf (cdr (assoc degree dist)))
		       (push (cons degree 1) dist))))
	     (nodes graph))
    (sort dist #'< :key 'car)))

(defun reconstruct-path (prev end)
  "Helper function for find-shortest-path;  walks the shortest path and
returns it as a list of edges as pairs of nodes."
  (when (cdr (assoc end prev))
    (cons (list (cdr (assoc end prev)) end)
	  (reconstruct-path prev (cdr (assoc end prev))))))

(defmethod find-shortest-path ((graph graph) (n1 integer) (n2 integer))
  "Dijkstra's algorithm for finding the shortest path between two nodes."
  (let ((nodes (node-ids graph)))
    (let ((distances (mapcar (lambda (n) (cons n most-positive-fixnum)) nodes))
	  (previous (mapcar (lambda (n) (cons n nil)) nodes)))
      (setf (cdr (assoc n1 distances)) 0)
      (loop until (null nodes) do
	   (setf distances (sort distances '< :key 'cdr))
	   (let ((next (first (remove-if-not (lambda (d)
                                               (member (car d) nodes))
					     distances))))
	     (when (= (cdr next) most-positive-fixnum)
	       (return nil))
	     (when (= (car next) n2)
	       (return-from find-shortest-path
		 (nreverse (reconstruct-path previous n2))))
	     (setq nodes (remove (car next) nodes))
	     (dolist (neighbor (if (directed? graph)
				   (outbound-neighbors graph (car next))
				   (neighbors graph (car next))))
	       (let ((distance (1+ (cdr (assoc (car next) distances)))))
		 (when (< distance (cdr (assoc neighbor distances)))
		   (setf (cdr (assoc neighbor distances)) distance
			 (cdr (assoc neighbor previous)) (car next))))))))))

(defmethod find-shortest-path ((graph graph) n1 n2)
  (find-shortest-path graph
		      (gethash n1 (nodes graph))
		      (gethash n2 (nodes graph))))

(defmethod distance-map ((graph graph) (id integer) &key expand-ids?)
  "Generate a sorted distance map for the given node."
  (let ((map (list (cons id 0))) (queue nil))
    (dolist (neighbor (if (directed? graph)
			  (outbound-neighbors graph id)
			  (neighbors graph id)))
      (let ((pair (cons neighbor 1)))
	(push pair map)
	(push pair queue)))
    (loop until (null queue) do
	 (let ((pair (pop queue)))
	   (dolist (neighbor (neighbors graph (car pair)))
	     (unless (or (= neighbor id) (member neighbor map :key 'car))
	       (let ((pair (cons neighbor (1+ (cdr pair)))))
		 (push pair map)
		 (setq queue (nconc queue (list pair))))))))
    (if expand-ids?
	(mapcar (lambda (pair)
                  (cons (gethash (car pair) (ids graph)) (cdr pair)))
		(sort map #'< :key #'cdr))
	(sort map #'< :key #'cdr))))

(defmethod distance-map ((graph graph) value &key expand-ids?)
  (distance-map graph (gethash value (nodes graph)) :expand-ids? expand-ids?))

(defmethod find-components ((graph graph) &key (return-ids? t))
  "Find all components in the graph and return them as a list of lists."
  (let ((nodes (node-ids graph))
	(components nil))
    (loop until (null nodes) do
	 (let ((dmap (distance-map graph (pop nodes))) (component nil))
	   (dolist (pair dmap)
	     (setq nodes (remove (car pair) nodes))
	     (push (gethash (car pair) (ids graph)) component))
	   (when component
	     (push component components))))
    (if return-ids?
        (mapcar #'(lambda (component)
                    (mapcar #'(lambda (n)
                                (lookup-node graph n))
                            component))
                (sort components #'> :key #'length))
        (sort components #'> :key #'length))))

(defmethod calculate-shortest-paths ((graph graph))
  (let ((paths nil))
    (dotimes (i (row-count (matrix graph)))
      (loop
         for j from (if (directed? graph) 0 i) to (1- (col-count (matrix graph)))
         do
           (unless (= i j)
             (push (list i j (find-shortest-path graph i j)) paths))))
    (nreverse paths)))

(defmethod cluster ((graph graph) (method (eql :edge-betweenness))
		    &key (edge-removal-count 0))
  "The clustering algorithm here is based on a metric called 'edge
betweenness'. It counts how many shortest paths in the network include a
given edge. An edge with high betweenness is one that is likely to separate
dense areas of the graph."
  (let* ((shortest-paths (calculate-shortest-paths graph))
	 (between-table (sort
			 (map-edges
			  #'(lambda (i j w)
                              (declare (ignore w))
			      (let ((coord (list i j)))
				(list coord
				      (reduce #'+ (mapcar
						   #'(lambda (p)
						       (count coord
							      (rest (third p))
							      :test 'equal))
						   shortest-paths)))))
			  graph :collect? t)
			 #'> :key #'second))
	 (removed-edges nil))
    (dotimes (i edge-removal-count)
      (let ((edge (pop between-table)))
	(push edge removed-edges)
	(delete-edge graph (first (first edge)) (second (first edge)))))
    (nreverse (mapcar #'(lambda (edge)
			  (list (first (first edge))
				(second (first edge))
				(second edge)))
		      removed-edges))))

(defmethod score-edges ((graph graph) &key sort?)
  (let ((span-map nil))
    (map-edges #'(lambda (n1 n2 w)
                   (delete-edge graph n1 n2)
                   (push (list (list n1 n2)
                               (length (find-shortest-path graph n1 n2)))
                         span-map)
                   (add-edge graph n1 n2 :weight w))
	       graph)
    (if sort?
	(sort span-map #'> :key #'second)
	span-map)))

(defmethod cluster ((graph graph) (method (eql :edge-span))
		    &key (edge-removal-count 0))
  "Recall that the span of an edge is the distance between the two endpoints
of the edge after the edge is removed. An edge with high span is one that
links vertices that would otherwise be far apart. This method scores the
edges and clusters based on span."
  (let ((span-map (score-edges graph :sort? t)) (removed-edges nil))
    (dotimes (i edge-removal-count)
      (let ((edge (pop span-map)))
	(push edge removed-edges)
	(delete-edge graph (first (first edge)) (second (first edge)))))
    (nreverse (mapcar #'(lambda (edge)
			  (list (first (first edge))
				(second (first edge))
				(second edge)))
		      removed-edges))))

(defmethod compute-page-rank ((graph graph) &key (k 2) (scaling-factor 1)
                              initial-values)
  (assert (and (numberp scaling-factor)
               (>= scaling-factor 0)
               (<= scaling-factor 1)))
  (assert (directed? graph))
  (assert (> k 0))
  (let* ((node-count (node-count graph))
	 (page-rank (or (and (arrayp initial-values)
			     (= (length initial-values) (node-count graph))
			     initial-values)
			(make-array node-count
                                    :element-type 'number
                                    :initial-element (/ 1 node-count)))))
    (dotimes (step k)
      (dbg "page-rank is ~A" page-rank)
      (let ((rank-received (make-array node-count :element-type 'number
                                       :initial-element 0)))
	(dotimes (source node-count)
	  (let* ((out-links (outbound-neighbors graph source))
		 (count (length out-links)))
	    (if (= count 0)
		(setf (aref rank-received source)
		      (+ (aref page-rank source) (aref rank-received source)))
		(dolist (node out-links)
		  (setf (aref rank-received node)
			(+ (aref rank-received node)
			   (/ (aref page-rank source) count)))))))
	(when (< scaling-factor 1)
	  (dotimes (i node-count)
	    (setf (aref rank-received i)
                  (* (aref rank-received i) scaling-factor)))
	  (let ((pr (/ (- 1 scaling-factor) node-count)))
	    (dotimes (i node-count)
	      (setf (aref rank-received i) (+ (aref rank-received i) pr)))))
	(setq page-rank rank-received)))
    page-rank))

(defmethod compute-page-rank-distribution ((graph graph) &key page-rank
                                           (bin-count 2) (k 2)
                                           (scaling-factor 1))
  (unless (arrayp page-rank)
    (setq page-rank
          (compute-page-rank graph :k k :scaling-factor scaling-factor)))
  (let ((min most-positive-fixnum) (max 0))
    (map-nodes #'(lambda (name id)
		   (declare (ignore name))
		   (let ((rank (aref page-rank id)))
		     (when (> rank max)
		       (setq max rank))
		     (when (< rank min)
		       (setq min rank))))
	       graph)
    (if (= 0 max)
	(error "Got 0 for max pagerank value.  Cannot compute distribution.")
	(let ((bin-size (/ (- max min) bin-count))
              (bins nil)
              (bin-map (make-hash-table)))
	  (dotimes (i bin-count)
	    (push (list (+ min (* i bin-size)) (+ min (* (1+ i) bin-size)) 0)
                  bins))
	  (map-nodes #'(lambda (name id)
			 (declare (ignore name))
			 (let ((rank (aref page-rank id)))
			   (dolist (triple bins)
			     (when (and (>= rank (first triple))
					(<= rank (second triple)))
			       (setf (gethash id bin-map) (second triple))
			       (incf (third triple))))))
		     graph)
	  (values (mapcar #'(lambda (triple)
                              (list (second triple) (third triple)))
			  (sort bins #'> :key 'first))
		  bin-map)))))

(defmethod compute-hub-authority-values ((graph graph) &key (k 2) normalize?)
  "Return (values hubs authorities) for all nods in the graph."
  (let ((hub-values (map-nodes #'(lambda (name id)
				   (declare (ignore id))
				   (cons name 1))
			       graph :collect? t))
	(auth-values (map-nodes #'(lambda (name id)
				    (declare (ignore id))
				    (cons name 1))
				graph :collect? t)))
    (dotimes (i k)
      (map-nodes
       #'(lambda (name id)
           (let ((inbound-neighbors (inbound-neighbors graph id)))
             (setf (cdr (assoc name auth-values :test 'equal))
                   (reduce #'+
                           (mapcar #'(lambda (n)
                                       (cdr (assoc (lookup-node graph n)
                                                   hub-values
                                                   :test 'equal)))
                                   inbound-neighbors)))))
       graph)
      (map-nodes
       #'(lambda (name id)
           (let ((outbound-neighbors (outbound-neighbors graph id)))
             (setf (cdr (assoc name hub-values :test 'equal))
                   (reduce #'+
                           (mapcar #'(lambda (n)
                                       (cdr (assoc (lookup-node graph n)
                                                   auth-values
                                                   :test 'equal)))
                                   outbound-neighbors)))))
       graph))
    (multiple-value-bind (h a)
	(if normalize?
	    (let ((hub-sum (reduce #'+ hub-values :key #'cdr))
		  (auth-sum (reduce #'+ auth-values :key #'cdr)))
	      (values (mapcar #'(lambda (pair)
				  (cons (car pair)
					(/ (cdr pair) hub-sum)))
			      hub-values)
		      (mapcar #'(lambda (pair)
				  (cons (car pair)
					(/ (cdr pair) auth-sum)))
			      auth-values)))
	    (values hub-values auth-values))
      (values (sort h #'> :key #'cdr)
	      (sort a #'> :key #'cdr)))))

(defmethod compute-center-nodes ((graph graph))
  "Return the center nodes of the graph."
  (let ((max-paths nil))
    (dolist (v1 (list-nodes graph))
      (push (cons v1 most-negative-fixnum) max-paths)
      (dolist (v2 (list-nodes graph))
	(unless (eql v1 v2)
	  (let ((path-length (length (find-shortest-path graph v1 v2))))
	    (when (> path-length (cdr (assoc v1 max-paths)))
	      (setf (cdr (assoc v1 max-paths)) path-length))))))
    (let ((sorted-max-paths (sort max-paths #'< :key #'cdr)))
      (mapcar #'car
	      (remove-if-not #'(lambda (n)
				 (= (cdr n)
				    (cdr (first sorted-max-paths))))
			     sorted-max-paths)))))

(defmethod spanning-tree ((graph graph) &key root)
  (let* ((stack nil)
         (root (or root (random-node graph)))
         (tree (make-graph :directed? t
                           :node-comparator (comparator graph)))
         (seen (make-hash-table :test 'equal)))
    (add-node tree root)
    (setf (gethash root seen) t)
    (dolist (n (neighbors graph root :return-ids? nil))
      (add-node tree n)
      (add-edge tree root n)
      (push n stack))
    (loop until (null stack) do
         (let ((node (pop stack)))
           (setf (gethash node seen) t)
           (dolist (n (neighbors graph node :return-ids? nil))
             (unless (lookup-node tree n)
               (add-node tree n)
               (unless (or (member n stack :test 'equal)
                           (gethash n seen))
                 (pushnew n stack :test 'equal))
               (add-edge tree node n)))))
    (values tree root)))
