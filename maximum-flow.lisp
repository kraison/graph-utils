(in-package :graph-utils)

(defmethod push-flow-via-edges ((graph graph) path)
  (let ((min-cap (apply #'min (mapcar
                               #'(lambda (e)
                                   (capacity graph (nth 0 e) (nth 1 e)))
                               path))))
    (dbg "min-cap of ~A is ~A" path min-cap)
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
  (let ((flow 0) (gf (copy-graph graph)) (loops 0))
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
                                 (dbg "Removing ~A" edge)
                                 (setq l0-edges
                                       (remove edge l0-edges :test 'equalp))
                                 nil))))))))
             (loop until (null (member source l0-edges :key 'first)) do
                  (let ((path (find-path source)))
                    (multiple-value-bind (min-cap min-edges)
                        (minimum-capacity gf path)
                      (declare (ignore min-cap))
                      (incf flow (push-flow-via-edges gf path))
                      (setq l0-edges
                            (set-difference l0-edges
                                            min-edges
                                            :test 'equalp))))))))
    (dbg "Computed ~A loops" loops)
    flow))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer)
                              (algorithm (eql :edmond-karp)))
  "This implements the basic Ford-Fulkerson algorithm using the Edmond-Karp
formulation."
  (let ((flow 0) (gf (copy-graph graph)) (loops 0))
    (loop
       (incf loops)
       (let ((path (find-shortest-path gf source sink)))
         (if path
             (incf flow (push-flow-via-edges gf path))
             (progn
               (dbg "Computed ~A loops" loops)
               (return-from find-maximum-flow flow)))))))

(defmethod init-karzanov ((gf graph) nodes edges source sink)
  (let ((in (make-hash-table))
        (out (make-hash-table)))
    (dbg "K-INIT NODES: ~A" nodes)
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
                          source sink)
  (dbg "Pushing ~A through node ~A" cap node)
  (let ((q (make-empty-queue))
        (flows (make-hash-table)))
    (enqueue q node)
    (setf (gethash node flows) cap)
    (loop until (empty-queue? q) do
         (let* ((u (dequeue q)) (f0 (gethash u flows 0)))
           (dbg "  Push working on node U = ~A" u)
           (loop while (> f0 0) do
                (let* ((edge (find u l0-edges :key 'first)) (w (second edge)))
                  (unless edge (return))
                  (dbg "  Push working on EDGE ~A" edge)
                  (when (and (eql 0 (gethash w flows 0)) (not (eql w sink)))
                    (dbg "  Push enqueuing node ~A" w)
                    (enqueue q w))
                  (if (<= (capacity gf u w) f0) ;; Should this be layered cap?
                      (let ((fv (capacity gf u w)))
                        (decf-edge-weight gf u w fv)
                        (dbg "  New cap~A = ~A" edge (capacity gf u w))
                        (setq l0-edges
                              (remove (list u w) l0-edges :test 'equalp))
                        (dbg "  Push setting fl(~A) = ~A"
                             w (+ fv (gethash w flows 0)))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf f0 fv))
                      (let ((fv f0))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf-edge-weight gf u w f0)
                        (dbg "  New cap~A = ~A" edge (capacity gf u w))
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
  (values l0-nodes l0-edges (sort capacities #'< :key #'second)))

(defmethod karzanov-pull ((gf graph) node l0-nodes l0-edges capacities cap
                          source sink)
  (dbg "Pulling ~A through node ~A" cap node)
  (let ((q (make-empty-queue))
        (flows (make-hash-table)))
    (enqueue q node)
    (setf (gethash node flows) cap)
    (loop until (empty-queue? q) do
         (let* ((u (dequeue q)) (f0 (gethash u flows 0)))
           (dbg "  Pull working on node U = ~A" u)
           (loop while (> f0 0) do
                (let* ((edge (find u l0-edges :key 'second))
                       (w (first edge)))
                  (unless edge (return))
                  (dbg "  Pull working on EDGE ~A" edge)
                  (when (and (eql 0 (gethash w flows 0)) (not (eql w source)))
                    (dbg "  Pull enqueueing node ~A" w)
                    (enqueue q w))
                  (if (<= (capacity gf w u) f0)
                      (let ((fv (capacity gf w u)))
                        (dbg "  Pull deleting edge ~A,~A" w u)
                        (decf-edge-weight gf w u fv)
                        (dbg "  New cap~A = ~A" edge (capacity gf w u))
                        (setq l0-edges
                              (remove (list w u) l0-edges :test 'equalp))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf f0 fv))
                      (let ((fv f0))
                        (setf (gethash w flows) (+ fv (gethash w flows 0)))
                        (decf-edge-weight gf w u f0)
                        (dbg "  New cap~A = ~A" edge (capacity gf w u))
                        (setq f0 0)))))
           (let ((cap-u (find u capacities :key 'first)))
             (dbg "  Pull got cap(~A) = ~A" u cap-u)
             (setf (second cap-u) (- (second cap-u) (gethash u flows 0)))
             (dbg "  Pull set cap(~A) = ~A" u (second cap-u))
             (when (eq 0 (second cap-u))
               (dbg "  Removing node ~A" u)
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
    (values l0-nodes l0-edges (sort capacities #'< :key #'second))))

(defmethod karzanov-push-pull ((gf graph) node l0-nodes l0-edges capacities
                               cap source sink)
  (multiple-value-bind (l0-nodes l0-edges capacities)
      (karzanov-push gf node l0-nodes l0-edges capacities cap source sink)
    (karzanov-pull gf node l0-nodes l0-edges capacities cap source sink)))

(defmethod find-maximum-flow ((graph directed-graph) (source integer)
                              (sink integer)
                              (algorithm (eql :karzanov)))
  "This implements Karzanov's algorithm."
  (let ((flow 0) (gf (copy-graph graph)) (loops 0))
    (loop until (null (find-shortest-path gf source sink)) do
         (incf loops)
         (dbg "Karzanov loop ~A, flow is ~A" loops flow)
         (multiple-value-bind (l0-nodes l0-edges)
             (compute-layered-network gf source sink)
           (dbg "L0 = ~A , ~A" l0-nodes l0-edges)
           ;; Karzanov-saturation
           (let ((capacities (init-karzanov gf l0-nodes l0-edges source sink))
                 (f* 0))
             (dbg "CAPS: ~A" capacities)
             (loop until (null l0-nodes) do
                  (destructuring-bind (node cap) (first capacities)
                    (if (>= 0 (second (find node capacities :key 'first)))
                        (progn
                          (dbg "Deleting node ~A" node)
                          (setq l0-nodes (remove node l0-nodes)
                                capacities (rest capacities)))
                        (progn
                          (dbg "Using node ~A of cap ~A" node cap)
                          (multiple-value-setq
                              (l0-nodes l0-edges capacities)
                            (karzanov-push-pull gf node l0-nodes l0-edges
                                                capacities cap source sink))
                          (dbg "Incrementing f* (~A) by ~A" f* cap)
                          (incf f* cap)))))
             (dbg "Incrementing flow (~A) by ~A to ~A"
                     flow f*
                     (incf flow f*)))))
    (dbg "Computed ~A loops" loops)
    flow))

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
