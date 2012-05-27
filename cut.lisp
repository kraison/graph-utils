(in-package :graph-utils)

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
    (mapcar #'(lambda (edge)
                (subseq edge 0 2))
            removed-edges)))

(defmethod minimal-cut ((graph graph) &key (method :cluster))
  (let ((g (copy-graph graph)))
    (cond ((eq method :cluster)
           (values (minimal-cut! g) g)))))

