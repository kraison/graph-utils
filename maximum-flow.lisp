(in-package :graph-utils)

(defun position-of-edge (n1 n2 edge-list)
  (let ((p-edge (if (> n2 n1) (list n1 n2) (list n2 n1))))
    (let ((p (position p-edge
                       edge-list
                       :test #'(lambda (e1 e2)
                                 (and (eql (first e1) (first e2))
                                      (eql (second e1) (second e2)))))))
      (values p p-edge))))

(defmethod push-flow-via-edges ((graph graph) path edges-in-flow)
  (let ((min-cap (apply #'min (mapcar
                               #'(lambda (e)
                                   (capacity graph (nth 0 e) (nth 1 e)))
                               path))))
    ;;(dbg "min-cap of ~A is ~A" path min-cap)
    (dolist (edge path)
      (destructuring-bind (n1 n2) edge
        (multiple-value-bind (p p-edge)
            (position-of-edge n1 n2 edges-in-flow)
          (if p
              (incf (nth 2 (nth p edges-in-flow)) min-cap)
              (push (append p-edge (list min-cap)) edges-in-flow))
          (decf-edge-weight graph n1 n2 min-cap)
          (incf-edge-weight graph n2 n1 min-cap))))
    (values min-cap edges-in-flow)))

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
    (let ((reversed-net (mapcar #'reverse edges)))
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
  (let ((flow 0) (gf (copy-graph graph)) (loops 0) (edges-in-flow nil))
    (loop until (null (find-shortest-path gf source sink)) do
         (incf loops)
         (multiple-value-bind (l0-nodes l0-edges)
             (compute-layered-network gf source sink)
           (declare (ignore l0-nodes))
           ;; Dinic-saturation
           (labels
               ((find-path (node)
                  (let ((edge (find node l0-edges :key 'first)))
                    (cond
                      ((null edge) nil)
                      ((eql (second edge) sink) (list edge))
                      (t (let ((path (find-path (second edge))))
                           (if path
                               (append (list edge) path)
                               (progn
                                 (setq l0-edges
                                       (remove edge l0-edges :test 'equalp))
                                 nil))))))))
             (loop until (null (member source l0-edges :key 'first)) do
                  (let ((path (find-path source)))
                    (multiple-value-bind (min-cap min-edges)
                        (minimum-capacity gf path)
                      (declare (ignore min-cap))
                      (multiple-value-bind (pushed-flow eif)
                          (push-flow-via-edges gf path edges-in-flow)
                        (incf flow pushed-flow)
                        (setq edges-in-flow eif))
                      (setq l0-edges
                            (set-difference l0-edges
                                            min-edges
                                            :test 'equalp))))))))
    (dbg "Dinic computed ~A loops" loops)
    (values flow
            (sort edges-in-flow #'> :key 'third)
            gf)))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer)
                              (algorithm (eql :edmond-karp)))
  "This implements the basic Ford-Fulkerson algorithm using the Edmond-Karp
formulation."
  (let ((flow 0) (gf (copy-graph graph)) (loops 0) (edges-in-flow nil))
    (loop
       (incf loops)
       (let ((path (find-shortest-path gf source sink)))
         (if path
             (multiple-value-bind (pushed-flow eif)
                 (push-flow-via-edges gf path edges-in-flow)
               (incf flow pushed-flow)
               (setq edges-in-flow eif))
             (progn
               (dbg "Edmond-Karp computed ~A loops" loops)
               (return-from find-maximum-flow
                 (values flow
                         (sort edges-in-flow #'> :key 'third)
                         gf))))))))

(defmethod init-karzanov ((gf graph) nodes edges source sink)
  (let ((in (make-hash-table))
        (out (make-hash-table)))
    (dolist (node nodes)
      (setf (gethash node in) 0)
      (setf (gethash node out) 0))
    (setf (gethash sink out) most-positive-fixnum)
    (setf (gethash source in) most-positive-fixnum)
    (dolist (edge edges)
      (let ((cap (capacity gf (nth 0 edge) (nth 1 edge))))
        (incf (gethash (nth 1 edge) in 0) cap)
        (incf (gethash (nth 0 edge) out 0) cap)))
    (sort
     (mapcar #'(lambda (node)
                 (let ((capacity (min (gethash node out) (gethash node in))))
                   (list node capacity)))
             nodes)
     #'< :key #'second)))

(defmethod karzanov-push ((gf graph) node l0-nodes l0-edges capacities cap
                          source sink edges-in-flow)
  (let ((q (make-empty-queue)) (flows (make-hash-table)))
    (enqueue q node)
    (setf (gethash node flows) cap)
    (loop until (empty-queue? q) do
         (let* ((u (dequeue q)) (f0 (gethash u flows 0)))
           (loop while (> f0 0) do
                (let* ((edge (find u l0-edges :key 'first)) (w (second edge)))
                  (unless edge (return))
                  (when (and (eql 0 (gethash w flows 0)) (not (eql w sink)))
                    (enqueue q w))
                  (if (<= (capacity gf u w) f0)
                      (let ((fv (capacity gf u w)))
                        (multiple-value-bind (p p-edge)
                            (position-of-edge u w edges-in-flow)
                          (if p
                              (incf (nth 2 (nth p edges-in-flow)) fv)
                              (push (append p-edge (list fv))
                                    edges-in-flow)))
                        (decf-edge-weight gf u w fv)
                        (setq l0-edges
                              (remove (list u w) l0-edges :test 'equalp))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf f0 fv))
                      (let ((fv f0))
                        (multiple-value-bind (p p-edge)
                            (position-of-edge u w edges-in-flow)
                          (if p
                              (incf (nth 2 (nth p edges-in-flow)) f0)
                              (push (append p-edge (list f0))
                                    edges-in-flow)))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf-edge-weight gf u w f0)
                        (setq f0 0)))))
           (when (not (eql u node))
             (let ((cap-u (find u capacities :key 'first)))
               (setf (second cap-u) (- (second cap-u) (gethash u flows 0)))
               (when (eq 0 (second cap-u))
                 (setq l0-nodes (remove u l0-nodes))
                 (setq l0-edges (remove u l0-edges :key 'second))
                 (setq l0-edges (remove u l0-edges :key 'first))
                 ;; Remove dead vertices
                 (dolist (n l0-nodes)
                   (when (or (null (find n l0-edges :key 'first))
                             (null (find n l0-edges :key 'second)))
                     (let ((cap-n (find n capacities :key 'first)))
                       (when cap-n
                         (setf (second cap-n) 0)))))
                 ))))))
  (values l0-nodes l0-edges
          (sort capacities #'< :key #'second) edges-in-flow))

(defmethod karzanov-pull ((gf graph) node l0-nodes l0-edges capacities cap
                          source sink edges-in-flow)
  (let ((q (make-empty-queue))
        (flows (make-hash-table)))
    (enqueue q node)
    (setf (gethash node flows) cap)
    (loop until (empty-queue? q) do
         (let* ((u (dequeue q)) (f0 (gethash u flows 0)))
           (loop while (> f0 0) do
                (let* ((edge (find u l0-edges :key 'second))
                       (w (first edge)))
                  (unless edge (return))
                  (when (and (eql 0 (gethash w flows 0)) (not (eql w source)))
                    (enqueue q w))
                  (if (<= (capacity gf w u) f0)
                      (let ((fv (capacity gf w u)))
                        (multiple-value-bind (p p-edge)
                            (position-of-edge u w edges-in-flow)
                          (if p
                              (incf (nth 2 (nth p edges-in-flow)) fv)
                              (push (append p-edge (list fv))
                                    edges-in-flow)))
                        (decf-edge-weight gf w u fv)
                        (setq l0-edges
                              (remove (list w u) l0-edges :test 'equalp))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf f0 fv))
                      (let ((fv f0))
                        (multiple-value-bind (p p-edge)
                            (position-of-edge u w edges-in-flow)
                          (if p
                              (incf (nth 2 (nth p edges-in-flow)) f0)
                              (push (append p-edge (list f0))
                                    edges-in-flow)))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf-edge-weight gf w u f0)
                        (setq f0 0)))))
           (let ((cap-u (find u capacities :key 'first)))
             (setf (second cap-u) (- (second cap-u) (gethash u flows 0)))
             (when (eq 0 (second cap-u))
               (setq l0-nodes (remove u l0-nodes))
               (setq l0-edges (remove u l0-edges :key 'second))
               (setq l0-edges (remove u l0-edges :key 'first))
               ;; Remove dead vertices
               (dolist (n l0-nodes)
                 (when (or (null (find n l0-edges :key 'first))
                           (null (find n l0-edges :key 'second)))
                   (let ((cap-n (find n capacities :key 'first)))
                     (when cap-n
                       (setf (second cap-n) 0)))))))))
    (values l0-nodes l0-edges
            (sort capacities #'< :key #'second) edges-in-flow)))

(defmethod karzanov-push-pull ((gf graph) node l0-nodes l0-edges capacities
                               cap source sink edges-in-flow)
  (multiple-value-bind (l0-nodes l0-edges capacities edges-in-flow)
      (karzanov-push gf node l0-nodes l0-edges capacities cap source sink
                     edges-in-flow)
    (karzanov-pull gf node l0-nodes l0-edges capacities cap source sink
                   edges-in-flow)))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer)
                              (algorithm (eql :karzanov)))
  "This implements Karzanov's algorithm."
  (let ((flow 0) (gf (copy-graph graph)) (loops 0) (edges-in-flow nil))
    (loop until (null (find-shortest-path gf source sink)) do
         (incf loops)
         (multiple-value-bind (l0-nodes l0-edges)
             (compute-layered-network gf source sink)
           ;; Karzanov-saturation
           (let ((capacities (init-karzanov gf l0-nodes l0-edges source sink))
                 (f* 0))
             (loop until (or (null l0-nodes) (not (member sink l0-nodes))) do
                  (destructuring-bind (node cap) (first capacities)
                    (if (>= 0 (second (find node capacities :key 'first)))
                        (progn
                          (setq l0-nodes (remove node l0-nodes)
                                l0-edges (remove node l0-edges :key 'first)
                                l0-edges (remove node l0-edges :key 'second)
                                capacities (rest capacities)))
                        (progn
                          (multiple-value-setq
                              (l0-nodes l0-edges capacities edges-in-flow)
                            (karzanov-push-pull gf node l0-nodes l0-edges
                                                capacities cap source sink
                                                edges-in-flow))
                          (incf f* cap)))))
             (incf flow f*))))
    (dbg "Karzanov computed ~A loops" loops)
    (values flow
            (sort edges-in-flow #'> :key 'third)
            gf)))

(defmethod expand-node-out ((graph graph) node cap)
  (let ((new-node (add-node graph (gensym "V"))))
    (dolist (neighbor (outbound-neighbors graph node))
      (add-edge graph new-node neighbor
                :weight (edge-weight graph node neighbor))
      (delete-edge graph node neighbor))
    (add-edge graph node new-node :weight cap)))

(defmethod expand-node-in ((graph graph) node cap)
  (let ((new-node (add-node graph (gensym "V"))))
    (dolist (neighbor (inbound-neighbors graph node))
      (add-edge graph neighbor new-node
                :weight (edge-weight graph neighbor node))
      (delete-edge graph neighbor node))
    (add-edge graph new-node node :weight cap)))

(defmethod expand-node-capacities ((graph graph) &optional source sink)
  (let ((g-prime (copy-graph graph)))
    (if (and source sink)
        (maphash
         #'(lambda (node cap)
             (cond ((eq node source)
                    ;; We've got to expand this one differently
                    ;; in order to preserve the original id as source
                    (expand-node-out g-prime node cap))
                   (t
                    (expand-node-in graph node cap))))
                 (node-caps g-prime))
        (maphash #'(lambda (node cap)
                     (expand-node-in graph node cap))
                 (node-caps g-prime)))
    (clrhash (node-caps g-prime))
    g-prime))

(defmethod compute-maximum-flow ((graph directed-graph) (source integer)
                                 (sink integer) &key algorithm
                                 node-capacities?)
  (if node-capacities?
      (let ((g-prime (expand-node-capacities graph source sink)))
        (find-maximum-flow g-prime source sink (or algorithm :karzanov)))
      (find-maximum-flow graph source sink (or algorithm :karzanov))))

(defmethod compute-maximum-flow ((graph directed-graph) source sink
                                 &key algorithm node-capacities?)
  "Compute max flow for the directed graph.  Algorithm can be one of
:edmond-karp, :dinic or :karzanov"
  (compute-maximum-flow graph
                        (lookup-node graph source)
                        (lookup-node graph sink)
                        :algorithm (or algorithm :karzanov)
                        :node-capacities? node-capacities?))
