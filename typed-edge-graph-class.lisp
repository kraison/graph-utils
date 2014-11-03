(in-package #:graph-utils)

(declaim (optimize (speed 3) (space 2)))

(defclass typed-graph (directed-graph)
  ((matrix :accessor matrix :initarg :matrix
           :initform (make-hash-table :test 'eql))
   (edge-type-comparator :accessor edge-type-comparator
                         :initarg :edge-type-comparator :initform 'eql)
   (indices :accessor indices :initarg :indices
            :initform (make-hash-table :test 'equalp))))

(defgeneric typed-graph? (thing)
  (:documentation "typed graph predicate")
  (:method ((thing typed-graph)) t)
  (:method (thing) nil))

(defmethod print-object ((graph typed-graph) stream)
  "Print a typed-graph"
  (print-unreadable-object (graph stream :type t)
    (format stream "~A edge types (~A vertices & ~A edges)"
            (hash-table-count (matrix graph))
	    (node-count graph)
            (edges graph))))

(defmethod edge-types ((graph typed-graph))
  (loop for et being the hash-keys in (matrix graph) collecting et))

(defmethod add-edge-type ((graph typed-graph) edge-type)
  (or (gethash edge-type (matrix graph))
      (setf (gethash edge-type (matrix graph))
            (make-sparse-array (list (node-count graph) (node-count graph))
                               :adjustable t
                               :element-type 'number
                               :initial-element 0))))

(defmethod add-edge-index ((graph typed-graph) edge-type index-type unique? ordered? ordering-fn)
  (add-edge-type graph edge-type)
  (case index-type
    (:sp
     (cond ((and ordered? unique?)
            (setf (gethash (cons edge-type :weight) (indices graph))
                  (make-index :type 'unique-ordered-index
                              :key-equality-fn (lambda (t1 t2)
                                                 (and (funcall (comparator graph)
                                                               (subject t1) (subject t2))
                                                      (eql (predicate t1) (predicate t2))))
                              :value-equality-fn (comparator graph)
                              :ordering-fn ordering-fn
                              :edge-type edge-type)))
           ((and (null ordered?) (null unique?))
            (setf (gethash (cons edge-type :weight) (indices graph))
                  (make-index :type 'index
                              :key-equality-fn (comparator graph)
                              :value-equality-fn (comparator graph)
                              :edge-type edge-type)))))
    (otherwise
     (error "Only these index types are available: :sp :spw "))))

(defun make-typed-graph (&key (node-comparator 'equal) (saturation-point 0)
                         (edge-type-comparator 'eql) initial-edge-types)
  "Create a new typed-graph object. You are responsible for making sure that
node-comparator is a valid hash table test."
  (let ((g (make-instance 'typed-graph
                          :comparator node-comparator
                          :edge-type-comparator edge-type-comparator
                          :s-point saturation-point
                          :matrix (make-hash-table :test edge-type-comparator)
                          :nodes (make-hash-table :test node-comparator))))
    (dolist (e initial-edge-types)
      (add-edge-type g e))
    g))

(defmethod add-node ((graph typed-graph) value &key capacity)
  "Add a node to the graph."
  (or (gethash value (nodes graph))
      (let ((id (incf (last-id graph))))
        (maphash (lambda (etype matrix)
                   (declare (ignore etype))
                   (incf-sarray-dimensions matrix))
                 (matrix graph))
        (when capacity
          (setf (gethash id (node-caps graph)) capacity))
        (setf (gethash id (in-degree-table graph)) 0
              (gethash id (out-degree-table graph)) 0
              (gethash id (degree-table graph)) 0
              (gethash value (nodes graph)) id
              (gethash id (ids graph)) value)
        id)))

(defmethod neighbors ((graph typed-graph) (node integer) &key edge-type
                      (return-ids? t))
  "Return a list of ids for this node's neighbors. Returns inbound and
outbound neighbors for a directed graph."
  (let ((neighbors nil))
    (flet ((find-neighbors (matrix etype)
             (map-sarray-col (lambda (row-id value)
                               (when (> value 0)
                                 (push (cons etype row-id) neighbors)))
                             matrix node)
             (map-sarray-row (lambda (col-id value)
                               (when (> value 0)
                                 (push (cons etype col-id) neighbors)))
                             matrix node)))
      (if edge-type
          (find-neighbors (gethash edge-type (matrix graph)) edge-type)
          (maphash (lambda (etype matrix)
                     (find-neighbors matrix etype))
                   (matrix graph)))
      (if return-ids?
          (nreverse neighbors)
          (mapcar (lambda (pair)
                    (lookup-node graph (cdr pair)))
                  (nreverse neighbors))))))

(defmethod neighbors ((graph typed-graph) node &key edge-type (return-ids? t))
  "Return a list of ids for this node's neighbors."
  (neighbors graph
             (gethash node (nodes graph))
             :edge-type edge-type
             :return-ids? return-ids?))

(defmethod inbound-neighbors ((graph typed-graph) node &key edge-type
                              (return-ids? t))
  (inbound-neighbors graph
                     (gethash node (nodes graph))
                     :edge-type edge-type
                     :return-ids? return-ids?))

(defmethod inbound-neighbors ((graph typed-graph) (node integer) &key edge-type
                              (return-ids? t))
  (let ((neighbors nil))
    (flet ((find-neighbors (matrix etype)
             (map-sarray-col (lambda (row-id value)
                               (when (> value 0)
                                 (push (cons etype row-id) neighbors)))
                             matrix node)))
      (if edge-type
          (find-neighbors (gethash edge-type (matrix graph)) edge-type)
          (maphash (lambda (etype matrix)
                     (find-neighbors matrix etype))
                   (matrix graph)))
      (if return-ids?
          (nreverse neighbors)
          (mapcar (lambda (pair)
                    (lookup-node graph (cdr pair)))
                  (nreverse neighbors))))))

(defmethod outbound-neighbors ((graph typed-graph) node &key edge-type
                               (return-ids? t))
  (outbound-neighbors graph
                      (gethash node (nodes graph))
                      :edge-type edge-type
                      :return-ids? return-ids?))

(defmethod outbound-neighbors ((graph typed-graph) (node integer) &key
                               edge-type (return-ids? t))
  (let ((neighbors nil))
    (flet ((find-neighbors (matrix etype)
             (map-sarray-row #'(lambda (col-id value)
                                 (when (> value 0)
                                   (push (cons etype col-id) neighbors)))
                             matrix node)))
      (if edge-type
          (find-neighbors (gethash edge-type (matrix graph)) edge-type)
          (maphash #'(lambda (etype matrix)
                       (find-neighbors matrix etype))
                   (matrix graph)))
      (if return-ids?
          (nreverse neighbors)
          (mapcar #'(lambda (pair)
                      (lookup-node graph (cdr pair)))
                  (nreverse neighbors))))))

(defmethod edge-exists? ((graph typed-graph) (n1 integer) (n2 integer)
                         &key edge-type)
  "Is there an edge between n1 and n2 of type edge-type?"
  (let ((matrix (gethash edge-type (matrix graph))))
    (handler-case
        (when (and (sparse-array? matrix)
                   (numberp (saref matrix n1 n2))
                   (> (saref matrix n1 n2) 0))
          (saref matrix n1 n2))
      (error (c)
        (ignore-errors
          (dbg "Problem with edge (~A,~A)->~A: ~A" n1 n2 (saref matrix n1 n2) c))
        nil))))

(defmethod edge-exists? ((graph typed-graph) n1 n2 &key edge-type)
  "Is there an edge between n1 and n2 of type edge-type?"
  (let ((node1 (lookup-node graph n1))
        (node2 (lookup-node graph n2)))
    (when (and node1 node2)
      (edge-exists? graph node1 node2 :edge-type edge-type))))

(defmethod add-edge ((graph typed-graph) (n1 integer) (n2 integer) &key
                     (weight 1) edge-type)
  "Add an edge between n1 and n2 of type edge-type."
  (unless (= n1 n2)
    (let ((matrix (gethash edge-type (matrix graph))))
      (unless (sparse-array? matrix)
        (setq matrix (add-edge-type graph edge-type)))
      (unless (> (saref matrix n1 n2) 0)
        (incf (gethash n1 (out-degree-table graph)))
        (incf (gethash n2 (in-degree-table graph)))
        (incf (edges graph)))
      (setf (saref matrix n1 n2) weight)
      (list n1 n2 edge-type))))

(defmethod add-edge ((graph typed-graph) n1 n2 &key (weight 1) edge-type)
  "Add an edge between n1 and n2 of type edge-type."
  (let ((node1 (or (lookup-node graph n1) (add-node graph n1)))
        (node2 (or (lookup-node graph n2) (add-node graph n2))))
    (when (and node1 node2)
      (add-edge graph
                node1
                node2
                :edge-type edge-type
                :weight weight))))

(defmethod delete-edge ((graph typed-graph) (n1 integer) (n2 integer)
                        &optional edge-type)
  "Remove an edge from the graph."
  (unless (= n1 n2)
    (let ((matrix (gethash edge-type (matrix graph))))
      (when (sparse-array? matrix)
        (when (> (saref matrix n1 n2) 0)
          (decf (gethash n1 (out-degree-table graph)))
          (decf (gethash n2 (in-degree-table graph)))
          (decf (edges graph))
          (setf (saref matrix n1 n2) 0))))))

(defmethod delete-edge ((graph typed-graph) n1 n2 &optional edge-type)
  (let ((node1 (or (lookup-node graph n1) (add-node graph n1)))
        (node2 (or (lookup-node graph n2) (add-node graph n2))))
    (when (and node1 node2)
      (delete-edge graph node1 node2 edge-type))))

(defmethod map-edges ((fn function) (graph typed-graph) &key
                      edge-type collect? remove-nulls?)
  "Apply a function to all edges (possibly only of a single type)."
  (let ((r nil))
    (flet ((map-it (matrix etype)
             (when matrix
               (fast-map-sarray #'(lambda (n1 n2 w)
                                    (let ((v (funcall fn n1 n2 w etype)))
                                      (when collect?
                                        (push v r))))
                                ;;(push (list n1 n2 v etype) r))))
                                matrix))))
      (if edge-type
          (map-it (gethash edge-type (matrix graph)) edge-type)
          (maphash #'(lambda (etype matrix)
                       (map-it matrix etype))
                   (matrix graph)))
      (nreverse (if remove-nulls?
                    (remove-if #'(lambda (triple)
                                   (null (third triple)))
                               r)
                    r)))))

(defmethod list-edges ((graph typed-graph) &key nodes-as-ids edge-type)
  "Return all edges as pairs of nodes."
  (let ((r nil))
    (flet ((map-it (matrix etype)
             (when matrix
               (fast-map-sarray #'(lambda (n1 n2 w)
                                    (declare (ignore w))
                                    (push (if nodes-as-ids
                                              (list n1 n2 etype)
                                              (list (gethash n1 (ids graph))
                                                    (gethash n2 (ids graph))
                                                    etype))
                                          r))
                                matrix))))
      (if edge-type
          (map-it (gethash edge-type (matrix graph)) edge-type)
          (maphash #'(lambda (etype matrix)
                       (map-it matrix etype))
                   (matrix graph)))
    (nreverse r))))

(defmethod set-edge-weight ((graph typed-graph) (n1 integer) (n2 integer) weight
                            &key edge-type)
  (let ((matrix (gethash edge-type (matrix graph))))
    (setf (saref matrix n1 n2) weight)))

(defmethod edge-weight ((graph typed-graph) (n1 integer) (n2 integer)
                        &optional edge-type)
  (let ((matrix (gethash edge-type (matrix graph))))
    (saref matrix n1 n2)))

(defmethod edge-weight ((graph typed-graph) n1 n2 &optional edge-type)
  (edge-weight graph
               (lookup-node graph n1)
               (lookup-node graph n2)
               edge-type))

(defmethod incf-edge-weight ((graph typed-graph) (n1 integer) (n2 integer)
                             &key edge-type (delta 1))
  (let ((matrix (gethash edge-type (matrix graph))))
    (incf-sarray matrix (list n1 n2) delta)))

(defmethod incf-edge-weight ((graph typed-graph) n1 n2 &key edge-type delta)
  (incf-edge-weight graph
                    (gethash n1 (nodes graph))
                    (gethash n2 (nodes graph))
                    :edge-type edge-type
                    :delta delta))

(defmethod decf-edge-weight ((graph typed-graph) (n1 integer) (n2 integer)
                             &key edge-type (delta 1))
  (let ((matrix (gethash edge-type (matrix graph))))
    (decf-sarray matrix (list n1 n2) delta)))

(defmethod decf-edge-weight ((graph typed-graph) n1 n2 &key edge-type delta)
  (decf-edge-weight graph
                    (gethash n1 (nodes graph))
                    (gethash n2 (nodes graph))
                    :edge-type edge-type
                    :delta delta))

(defmethod random-edge ((graph typed-graph) &optional edge-type)
  (let ((n1 nil) (n2 nil) (w 0) (edge-types (edge-types graph)))
    (loop until (> w 0) do
         (let* ((edge-type (or edge-type
                               (elt edge-types (random (length edge-types)))))
                (matrix (gethash edge-type (matrix graph))))
           (setq n1 (random (row-count matrix))
                 n2 (random (col-count matrix))
                 w (saref matrix n1 n2))))
    (list n1 n2)))

(defmethod swap-edges ((graph typed-graph) e1 e2)
  (unless (and (= 3 (length e1)) (= 3 (length e2)))
    (error "Edges must be typed in a typed graph."))
  (apply #'delete-edge (cons graph e1))
  (apply #'delete-edge (cons graph e2))
  (add-edge graph (first e1) (first e2) :edge-type (third e1))
  (add-edge graph (second e1) (second e2) :edge-type (third e2)))

(defmethod reverse-edge ((graph typed-graph) n1 n2 &optional edge-type)
  (let ((weight (edge-weight graph n1 n2 edge-type)))
    (delete-edge graph n1 n2 edge-type)
    (add-edge graph n2 n1 :edge-type edge-type :weight weight)))

(defmethod reverse-all-edges ((graph typed-graph))
  (dolist (edge (list-edges graph :nodes-as-ids t))
    (let ((weight (edge-weight graph (first edge) (second edge) (third edge))))
      (delete-edge graph (first edge) (second edge) (third edge))
      (add-edge graph (second edge) (first edge)
                :weight weight
                :edge-type (third edge))))
  graph)

