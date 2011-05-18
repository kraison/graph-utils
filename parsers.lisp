(in-package #:graph-utils)

(defun parse-data (file)
  "Parse a .net file and make a graph out of it."
  (let ((graph (make-graph :directed? nil))
        (vertices? nil)
        (arcs? nil)
        (index (make-hash-table :test 'equal)))
    (with-open-file (in file :direction :input)
      (do ((line (read-line in nil :eof) (read-line in nil :eof)))
          ((eql line :eof))
        (setq line (regex-replace "^\\s+" line ""))
        (cond ((scan "^\*[Vv]ertices" line) 
	       ;; FIXME: parse the vertex count for later verification
	       (setq vertices? t arcs? nil))
              ((scan "^\*[Aa]rcs" line)
               (adjust-adjacency-matrix graph)
               (setq arcs? t vertices? nil))
              ((scan "^\*[Ee]dgeslist" line)
               (adjust-adjacency-matrix graph)
	       (setf (directed? graph) t)
               (setq arcs? t vertices? nil))
              (vertices?
               (destructuring-bind (id value &optional n1 n2 n3) 
		   ;; FIXME: need better splitting;  spaces in names!
		   (split "\\s+" line)
                 (declare (ignore n1 n2 n3))
                 (setq value (regex-replace-all "\"" value ""))
                 (setf (gethash id index) value)
                 (add-node graph value :no-expand? t)))
              (arcs?
               (destructuring-bind (source &rest destinations) (split "\\s+" line)
		 (dolist (d destinations)
		   (add-edge graph 
			     (or (gethash source index) source)
			     (or (gethash d index) d))
		   (when (not (directed? graph))
		     (add-edge graph 
			       (or (gethash d index) d)
			       (or (gethash source index) source)))))))))
    graph))
