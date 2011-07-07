(in-package #:graph-utils)

(declaim (optimize (speed 3) (space 2)))

(defmethod density ((graph graph))
  "Calculate the graph's density."
  (coerce
   (/ (edge-count graph) 
      (/ (* (node-count graph) (- (node-count graph) 1)) 2))
   'float))

(defmethod degree ((graph graph) node)
  (degree graph (gethash node (nodes graph))))

(defmethod degree ((graph graph) (node integer))
  "Calculate the degree of a node."
  (if (undirected? graph)
      (let ((degree 0))
	(loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	     (when (and (not (= i node)) (not (zerop (aref (matrix graph) node i))))
	       (incf degree)))
	degree)
      (error "Cannot calculate the degree in a directed graph.  Use in-degree or out-degree instead.")))

(defmethod in-degree ((graph graph) (node integer))
  (if (directed? graph)
      (let ((degree 0))
	(loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
	     (when (> (aref (matrix graph) i node) 0)
	       (incf degree)))
	degree)
      (error "Cannot calculate in-degree on an undirected graph")))

(defmethod out-degree ((graph graph) (node integer))
  (if (directed? graph)
      (let ((degree 0))
	(loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	     (when (> (aref (matrix graph) node i) 0)
	       (incf degree)))
	degree)
      (error "Cannot calculate out-degree on an undirected graph")))

(defmethod degree-distribution ((graph graph))
  "Generate the degree distribution for the graph. For a directed graph, returns the out-degree 
distribution."
  (let ((dist nil))
    (maphash #'(lambda (node id)
		 (declare (ignore node))
		 (let ((degree 0))
		   (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
			(when (not (zerop (aref (matrix graph) id i)))
			  (incf degree)))
		   (if (assoc degree dist)
		       (incf (cdr (assoc degree dist)))
		       (push (cons degree 1) dist))))
	     (nodes graph))
    (sort dist #'< :key 'car)))

(defmethod in-degree-distribution ((graph graph))
  "Generate the degree distribution for the graph. For a directed graph, returns the out-degree 
distribution."
  (unless (directed? graph)
    (error "Cannot compute in-degree-distribution on an undirected graph"))
  (let ((dist nil))
    (maphash #'(lambda (node id)
		 (declare (ignore node))
		 (let ((degree 0))
		   (loop for i from 0 to (1- (array-dimension (matrix graph) 1)) do
			(when (not (zerop (aref (matrix graph) i id)))
			  (incf degree)))
		   (if (assoc degree dist)
		       (incf (cdr (assoc degree dist)))
		       (push (cons degree 1) dist))))
	     (nodes graph))
    (sort dist #'< :key 'car)))

(defun reconstruct-path (prev end)
  "Helper function for find-shortest-path;  walks the shortest path and returns it
as a list of edges as pairs of nodes."
  (when (cdr (assoc end prev))
    (cons (list (cdr (assoc end prev)) end)
	  (reconstruct-path prev (cdr (assoc end prev))))))

(defmethod find-shortest-path ((graph graph) (n1 integer) (n2 integer))
  "Dijkstra's algorithm for finding the shortest path between two nodes."
  (let ((nodes (node-ids graph)))
    (let ((distances (mapcar #'(lambda (n) (cons n most-positive-fixnum)) nodes))
	  (previous (mapcar #'(lambda (n) (cons n nil)) nodes)))
      (setf (cdr (assoc n1 distances)) 0)
      (loop until (null nodes) do
	   (setf distances (sort distances #'< :key #'cdr))
	   (let ((next (first (remove-if-not #'(lambda (d)
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
	(mapcar #'(lambda (pair)
		    (cons (gethash (car pair) (ids graph)) (cdr pair)))
		(sort map #'< :key #'cdr))
	(sort map #'< :key #'cdr))))

(defmethod distance-map ((graph graph) (value string) &key expand-ids?)
  (distance-map graph (gethash value (nodes graph)) :expand-ids? expand-ids?))

(defmethod find-components ((graph graph))
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
    (sort components #'> :key #'length)))

(defun which (program)
  (cl-ppcre:regex-replace-all 
   "\\s+$"
   (trivial-shell:shell-command (format nil "/usr/bin/which ~A" program))
   ""))

(defmethod visualize ((graph graph) &key (file "/var/tmp/graph.dot") render? colors)
  "Save a dot file of this graph. Render can be one of (:heirarchical :circular :radial :spring),
which will render the graph using the appropriate Graphviz tool."
  (let ((memory (make-hash-table :test 'equalp)) 
	(connector (if (directed? graph) "->" "--")))
    (with-open-file (out file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "~A graphutils {~%" (if (directed? graph) "digraph" "graph"))
      (format out "  node [ color = black, fillcolor = while, style = filled ];~%")
      (map-nodes #'(lambda (name id)
		     (let ((neighbors (if (directed? graph)
					  (outbound-neighbors graph id)
					  (neighbors graph id))))
		       (dolist (n neighbors)
			 (unless (if (directed? graph)
				     (gethash (list id n) memory)
				     (or (gethash (list id n) memory)
					 (gethash (list n id) memory)))
			   (setf (gethash (list id n) memory) t)
			   (when (not (directed? graph))
			     (setf (gethash (list n id) memory) t))
			   (format out "  \"~A\" ~A \"~A\" [w=~A];~%" 
				   name
				   connector
				   (gethash n (ids graph))
				   (aref (matrix graph) id n))))
		       (format out "  \"~A\" [fillcolor=\"#~A\"];~%" 
			       name 
			       (if (hash-table-p colors)
				   (gethash name colors)
				   "ffff00"))))
		 graph)
      (format out "}~%"))
    (if render?
	(let ((f (regex-replace "\.[a-z]+$" file "\.png"))
	      (program (case render?
			 (:hierarchical (which "dot"))
			 (:circular     (which "circo"))
			 (:radial       (which "twopi"))
			 (:spring       (or (which "fdp") (which "neato")))
			 (otherwise     (or (which "fdp") (which "dot"))))))
	  (if program
	      (multiple-value-bind (output error-output exit-status)
		  (trivial-shell:shell-command (format nil "~A -Tpng -o ~A ~A" program f file))
		(unless (= 0 exit-status)
		  (error "~A exited with status ~A: ~A ~A~%" program exit-status output error-output)))
	      (format t "Unable to create PNG of graph ~A.  Graphviz not in your path.~%" graph))
	  f)
	file)))

#|
(defmethod generate-random-graph ((model (eql :rooted-tree)) (size integer)
				  &key directed? component-size density name-fn &allow-other-keys)
  (let* ((graph (make-graph :directed? directed?))
	 (next-node-id 1))
    (dotimes (i size)
      (add-node graph (funcall name-fn) :no-expand? t))
    (adjust-adjacency-matrix graph)
    (labels ((make-component (anchor)
	       (let ((start-node next-node-id))
		 (loop for i from next-node-id to (+ next-node-id component-size) do
		      (when (= next-node-id size)
			(return-from make-component))
		      (add-edge graph anchor next-node-id)
		      (incf next-node-id))
		 (loop for i from start-node to (+ start-node component-size) do
		      (make-component i)))))
      (make-component 0))
    graph))
|#

(defmethod generate-random-graph ((model (eql :erdos-renyi)) (size integer) 
				  &key p)
  (let ((graph (make-graph)))
    (dotimes (i size)
      (add-node graph i :no-expand? t))
    (adjust-adjacency-matrix graph)
    (dotimes (i size)
      (loop for j from (1+ i) to (1- size) do
	   (when (<= (random 1.0) p)
	     (add-edge graph i j))))
    graph))

(defmethod generate-random-graph ((model (eql :barabasi-albert)) (size integer)
				  &key (saturation-point 0) &allow-other-keys)
  (when (< size 4)
    (error "Cannot generate a barabasi-albert graph of size less than 4"))
  (let ((graph (make-graph :saturation-point saturation-point))
	(degree-table (make-array size :element-type 'integer :initial-element 0)))
    (dotimes (i 3)
      (add-node graph i))
    (dotimes (i 3)
      (loop for j from (1+ i) to 2 do
	   (incf (aref degree-table i))
	   (incf (aref degree-table j))
	   (add-edge graph i j)))
    (loop for i from 3 to (1- size) do
	 (add-node graph i :no-expand? t))
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

(defmethod calculate-shortest-paths ((graph graph))
  (let ((paths nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (loop for j from (if (directed? graph) 0 i) to 
	      (1- (array-dimension (matrix graph) 1)) 
	    do
	      (unless (= i j)
		(push (list i j (find-shortest-path graph i j)) paths))))
    (nreverse paths)))


(defmethod cluster ((graph graph) (method (eql :edge-betweenness))
		    &key (edge-removal-count 0))
  "The clustering algorithm here is based on a metric called 'edge betweenness'. It counts how many
shortest paths in the network include a given edge. An edge with high betweenness is one that is
likely to separate dense areas of the graph."
  (let* ((shortest-paths (calculate-shortest-paths graph))
	 (between-table (sort
			 (map-edges 
			  #'(lambda (i j)
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
			  (list (lookup-node graph (first (first edge)))
				(lookup-node graph (second (first edge)))
				(second edge)))
		      removed-edges))))

(defmethod score-edges ((graph graph) &key sort?)
  (let ((span-map nil))
    (map-edges #'(lambda (n1 n2)
		   (let ((w (edge-weight graph n1 n2)))
		     (delete-edge graph n1 n2)
		     (push (list (list n1 n2)
				 (length (find-shortest-path graph n1 n2)))
			   span-map)
		     (add-edge graph n1 n2 :weight w)))
	       graph)
    (if sort?
	(sort span-map #'> :key #'second)
	span-map)))

(defmethod cluster ((graph graph) (method (eql :edge-span)) 
		    &key (edge-removal-count 0))
  "Recall that the span of an edge is the distance between the two endpoints of the edge
after the edge is removed. An edge with high span is one that links vertices that would otherwise
be far apart. This method scores the edges and clusters based on span."
  (let ((span-map (score-edges graph :sort? t)) (removed-edges nil))
    (dotimes (i edge-removal-count)
      (let ((edge (pop span-map)))
	(push edge removed-edges)
	(delete-edge graph (first (first edge)) (second (first edge)))))
    (nreverse (mapcar #'(lambda (edge)
			  (list (lookup-node graph (first (first edge)))
				(lookup-node graph (second (first edge)))
				(second edge)))
		      removed-edges))))

(defmethod minimal-cut! ((graph graph))
  (let ((removed-edges nil))
    (labels ((cut (g)
	       (cond ((or (< (node-count g) 2)
			  (= 0 (edge-count g))
			  (>= (length (find-components g)) 2))
		      g)
		     (t
		      (push (first (cluster graph :edge-span 
						  :edge-removal-count 1)) 
			    removed-edges)
		      (cut graph)))))
      (cut graph))
    (nreverse removed-edges)))

(defmethod minimal-cut ((graph graph))
  (let ((g (copy-graph graph)))
    (values (minimal-cut! g) g)))

(defmethod compute-page-rank ((graph graph) &key (k 2) (scaling-factor 1) initial-values)
  (assert (and (numberp scaling-factor) (>= scaling-factor 0) (<= scaling-factor 1)))
  (assert (directed? graph))
  (assert (> k 0))
  (let* ((node-count (node-count graph))
	 (page-rank (or (and (arrayp initial-values) 
			     (= (length initial-values) (node-count graph)) 
			     initial-values)
			(make-array node-count :element-type 'number :initial-element (/ 1 node-count)))))
    (dotimes (step k)
      ;;(format t "page-rank is ~A~%" page-rank)
      (let ((rank-received (make-array node-count :element-type 'number :initial-element 0)))
	(dotimes (source node-count)
	  (let* ((out-links (outbound-neighbors graph source))
		 (count (length out-links)))
	    ;;(format t "~A (~A) has ~A outbound links~%" source (lookup-node graph source) count)
	    (if (= count 0)
		(setf (aref rank-received source) 
		      (+ (aref page-rank source) (aref rank-received source)))
		(dolist (node out-links)
		  ;;(format t "Adding ~A to page-rank of ~A (~A)~%" 
			  ;;(/ (aref page-rank source) count) node (lookup-node graph node))
		  (setf (aref rank-received node)
			(+ (aref rank-received node)
			   (/ (aref page-rank source) count)))))))
	(when (< scaling-factor 1)
	  (dotimes (i node-count)
	    (setf (aref rank-received i) (* (aref rank-received i) scaling-factor)))
	  (let ((pr (/ (- 1 scaling-factor) node-count)))
	    (dotimes (i node-count)
	      (setf (aref rank-received i) (+ (aref rank-received i) pr)))))
	(setq page-rank rank-received)))
    page-rank))

(defmethod compute-page-rank-distribution ((graph graph) &key page-rank (bin-count 2) (k 2) 
					   (scaling-factor 1))
  (unless (arrayp page-rank)
    (setq page-rank (compute-page-rank graph :k k :scaling-factor scaling-factor)))
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
	(let ((bin-size (/ (- max min) bin-count)) (bins nil) (bin-map (make-hash-table)))
	  (dotimes (i bin-count)
	    (push (list (+ min (* i bin-size)) (+ min (* (1+ i) bin-size)) 0) bins))
	  (map-nodes #'(lambda (name id)
			 (declare (ignore name))
			 (let ((rank (aref page-rank id)))
			   (dolist (triple bins)
			     (when (and (>= rank (first triple))
					(<= rank (second triple)))
			       (setf (gethash id bin-map) (second triple))
			       (incf (third triple))))))
		     graph)
	  (values (mapcar #'(lambda (triple) (list (second triple) (third triple)))
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
      (map-nodes #'(lambda (name id)
		     (let ((inbound-neighbors (inbound-neighbors graph id)))
		       ;;(format t "~A inbound:  ~A~%" name inbound-neighbors)
		       (setf (cdr (assoc name auth-values :test 'equal))
			     (reduce #'+ 
				     (mapcar #'(lambda (n)
						 (cdr (assoc (lookup-node graph n)
							     hub-values 
							     :test 'equal)))
					     inbound-neighbors)))))
		 graph)
      (map-nodes #'(lambda (name id)
		     (let ((outbound-neighbors (outbound-neighbors graph id)))
		       ;;(format t "~A outbound:  ~A~%" name outbound-neighbors)
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
	      ;;(format t "hub-sum: ~A, auth-sum: ~A~%" hub-sum auth-sum)
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



