(in-package #:graph)

(defmethod density ((graph graph))
  "Calculate the graph's density."
  (coerce
   (/ (edge-count graph) 
      (/ (* (node-count graph) (- (node-count graph) 1)) 2))
   'float))

(defmethod degree ((graph graph) (node integer))
  (let ((degree 0))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (when (and (not (= i node)) (not (zerop (aref (matrix graph) node i))))
	   (incf degree)))
    degree))

(defmethod degree-distribution ((graph graph))
  "Generate the degree distribution for the graph."
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

(defun reconstruct-path (prev end)
  "Helper function for find-shortest-path;  walks the shortest path and returns it as a list of edges as
pairs of nodes."
  (when (cdr (assoc end prev))
    (cons (list (cdr (assoc end prev)) end) (reconstruct-path prev (cdr (assoc end prev))))))

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
		 (reverse (reconstruct-path previous n2))))
	     (setq nodes (remove (car next) nodes))
	     (dolist (neighbor (neighbors graph (car next)))
	       (let ((distance (1+ (cdr (assoc (car next) distances)))))
		 (when (< distance (cdr (assoc neighbor distances)))
		   (setf (cdr (assoc neighbor distances)) distance
			 (cdr (assoc neighbor previous)) (car next))))))))))

(defmethod find-shortest-path ((graph graph) (n1 string) (n2 string))
  (find-shortest-path graph (gethash n1 (nodes graph)) (gethash n2 (nodes graph))))

(defmethod distance-map ((graph graph) (id integer) &key expand-ids?)
  "Generate a sorted distance map for the given node."
  (let ((map (list (cons id 0))) (queue nil))
    (dolist (neighbor (neighbors graph id))
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
	     ;;(format t "Component is ~A~%" component)
	     (push component components))))
    (sort components #'> :key #'length)))

(defmethod visualize ((graph graph) &key (file "/var/tmp/graph.dot") render?)
  "Save a dot file of this graph."
  (let ((memory (make-hash-table :test 'equalp)) 
	(connector (if (directed? graph) "->" "--")))
    (with-open-file (out file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out "~A csc495 {~%" (if (directed? graph) "digraph" "graph"))
      (map-nodes #'(lambda (name id)
		     (let ((neighbors (neighbors graph id)))
		       (if neighbors
			   (dolist (n neighbors)
			     (unless (if (directed? graph)
					 (gethash (list id n) memory)
					 (or (gethash (list id n) memory)
					     (gethash (list n id) memory)))
			       (setf (gethash (list id n) memory) t)
			       (format out "  \"~A\" ~A \"~A\" [w=~A];~%" 
				       name
				       connector
				       (gethash n (ids graph))
				       (aref (matrix graph) id n))))
			     (format out "  \"~A\";~%" name))))
		 graph)
      (format out "}~%"))
    (if render?
	(let ((f (regex-replace "\.[a-z]+$" file "\.png")))
	  (sb-ext:run-program "/usr/bin/dot" (list "-Tpng" "-o" f file))
	  f)
	file)))

(defmethod generate-random-graph ((model (eql :erdos-renyi)) (size integer) &key p)
  (let ((graph (make-graph)))
    (dotimes (i size)
      (add-node graph i :no-expand? t))
    (adjust-adjacency-matrix graph)
    (dotimes (i size)
      (loop for j from (1+ i) to (1- size) do
	   (when (<= (random 1.0) p)
	     (add-edge graph i j))))
    graph))

(defmethod generate-random-graph ((model (eql :barabasi-albert)) (size integer) &key
				  &allow-other-keys)
  (when (< size 4)
    (error "Cannot generate a barabasi-albert graph of size less than 4"))
  (let ((graph (make-graph)))
    (dotimes (i 3)
      (add-node graph i))
    (dotimes (i 3)
      (loop for j from (1+ i) to 2 do
	   (add-edge graph i j)))
    (loop for i from 3 to (1- size) do
	 (add-node graph i :no-expand? t))
    (adjust-adjacency-matrix graph)
    (loop for i from 3 to (1- size) do
	 (loop for j from 0 to (1- i) do
	      (when (and (/= i j)
			 (<= (random 1.0) 
			     (/ (1+ (degree graph i)) (+ (edge-count graph) (1+ i)))))
		(add-edge graph i j))))
    graph))

(defmethod calculate-shortest-paths ((graph graph))
  (let ((paths nil))
    (loop for i from 0 to (1- (array-dimension (matrix graph) 0)) do
	 (loop for j from (if (directed? graph) 0 i) to (1- (array-dimension (matrix graph) 1)) do
	      (unless (= i j)
		(push (list i j (find-shortest-path graph i j)) paths))))
  paths))


(defmethod cluster ((graph graph) (method (eql :edge-betweenness)) &key (edge-removal-count 0))
  (let* ((shortest-paths (calculate-shortest-paths graph))
	 (between-table (sort
			 (map-edges #'(lambda (i j)
					(let ((coord (list i j)))
					  (list coord
						(reduce #'+ (mapcar #'(lambda (p) 
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

(defmethod cluster ((graph graph) (method (eql :edge-span)) &key (edge-removal-count 0))
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

(defmethod minimal-cut ((graph graph))
  (let ((removed-edges nil))
    (labels ((cut (g)
	       (cond ((or (< (node-count g) 2)
			  (= 0 (edge-count g))
			  (>= (length (find-components g)) 2))
		      g)
		     (t
		      (push (first (cluster graph :edge-span :edge-removal-count 1)) removed-edges)
		      (cut graph)))))
      (cut graph))
    (nreverse removed-edges)))
