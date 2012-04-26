(in-package :graph-utils)


(defmethod push-flow ((graph graph) path)
  (let ((min-cap (apply #'min (mapcar
                               #'(lambda (e)
                                   (capacity graph (nth 0 e) (nth 1 e)))
                               path))))
    (dolist (edge path)
      (destructuring-bind (n1 n2) edge
        (decf-edge-weight graph n1 n2 min-cap)
        (incf-edge-weight graph n2 n1 min-cap)))
    min-cap))


(defmethod compute-layered-network ((graph graph) source sink)
  "Build the layered network of graph using tweaked BFS."
  (let* ((nodes nil) (edges nil)
         (distances (make-hash-table))
         (queue (make-empty-queue)))
    (dolist (node (node-ids graph))
      (setf (gethash node distances) most-positive-fixnum))
    (setf (gethash source distances) 0)
    (enqueue queue source)
    (loop until (empty-queue? queue) do
         (let ((v (dequeue queue)))
           (dolist (w (outbound-neighbors graph v))
             (cond ((= most-positive-fixnum (gethash w distances))
                    (enqueue queue w)
                    (setf (gethash w distances) (1+ (gethash v distances)))
                    (pushnew w nodes)
                    (pushnew (list v w) edges :test 'equalp))
                   ((= (gethash w distances) (1+ (gethash v distances)))
                    (pushnew (list v w) edges :test 'equalp))))))
    (let ((reversed-net (mapcar #'(lambda (edge)
                                    (list (second edge) (first edge)))
                                edges)))
      (let ((touched-nodes nil) (queue (make-empty-queue)))
        (enqueue queue sink)
        (loop until (empty-queue? queue) do
             (let* ((node (dequeue queue))
                    (children (mapcar #'second
                                      (remove-if-not
                                       #'(lambda (edge)
                                           (eq node (first edge)))
                                       reversed-net))))
               (pushnew node touched-nodes)
               (dolist (child children)
                 (unless (member child touched-nodes)
                   (enqueue queue child)))))
        (dolist (node (node-ids graph))
          (unless (member node touched-nodes)
            (setq nodes (remove node nodes))
            (setq edges (remove-if #'(lambda (edge)
                                       (member node edge))
                                   edges))))))
    (values nodes edges)))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer) (algorithm (eql :dinic)))
  "This implements Dinic's algorithm."
  (let ((flow 0) (gf (copy-graph graph)))
    (loop until (null (find-shortest-path gf source sink)) do
         (multiple-value-bind (nodes edges)
             (compute-layered-network gf source sink)
           (declare (ignore nodes))
           (loop until (null (member source edges :key 'first)) do
                (labels
                    ((find-path (node)
                       (let ((edge (find node edges :key 'first)))
                         (cond
                           ((null edge) nil)
                           ((eql (second edge) sink) (list edge))
                           (t (let ((path (find-path (second edge))))
                                (if path
                                    (append (list edge) path)
                                    (progn
                                      (setq edges
                                            (remove edge edges :test 'equalp))
                                      nil))))))))
                  (let ((path (find-path source)))
                    (multiple-value-bind (min-cap min-edges)
                        (minimum-capacity gf path)
                      (declare (ignore min-cap))
                      (incf flow (push-flow gf path))
                      (setq edges
                            (set-difference edges min-edges
                                            :test 'equalp))))))))
    flow))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer)
                              (algorithm (eql :edmond-karp)))
  "This implements the basic Ford-Fulkerson algorithm using the Edmond-Karp
formulation."
  (let ((flow 0) (gf (copy-graph graph)))
    (loop
       (let ((path (find-shortest-path gf source sink)))
         (if path
             (incf flow (push-flow gf path))
             (return-from find-maximum-flow flow))))))

(defmethod compute-maximum-flow ((graph directed-graph) (source integer)
                                 (sink integer) &optional algorithm)
  (find-maximum-flow graph source sink (or algorithm :edmond-karp)))

(defmethod compute-maximum-flow ((graph directed-graph) source sink
                                 &optional algorithm)
  "Compute max flow for the directed graph.  Algorithm can be one of
:edmond-karp, :dinic or :karzanov"
  (find-maximum-flow graph
                     (gethash source (nodes graph))
                     (gethash sink (nodes graph))
                     (or algorithm :edmond-karp)))
