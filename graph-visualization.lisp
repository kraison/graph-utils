(in-package #:graph-utils)

(defun which (program)
  (cl-ppcre:regex-replace-all
   "\\s+$"
   (trivial-shell:shell-command (format nil "/usr/bin/which ~A" program))
   ""))

(defmethod visualize ((graph graph) &key (file "/var/tmp/graph.dot") render?
                                      colors sizes (format "svg"))
  "Save a dot file of this graph. Render can be one of (:heirarchical
:circular :radial :spring), which will render the graph using the appropriate
Graphviz tool."
  (let ((memory (make-hash-table :test 'equalp))
	(connector (if (directed? graph) "->" "--")))
    (with-open-file (out file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out
              "~A graphutils~%{~%  splines=true;~%  node [ color = black, "
	      (if (directed? graph) "digraph" "graph"))
      (format out "fillcolor = white, style = filled ];~%")
      (map-nodes (lambda (name id)
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
                         (format out
                                 "  \"~A\" ~A \"~A\" [w=~A,label=~A];~%"
                                 name
                                 connector
                                 (gethash n (ids graph))
                                 (saref (matrix graph) id n)
                                 (saref (matrix graph) id n))))
                     (format out "  \"~A\" [fillcolor=\"~A\""
                             name
                             (if (hash-table-p colors)
                                 (gethash name colors)
                                 "#ffff00"))
                     (if (hash-table-p sizes)
                         (progn
                           (format out ",shape=box,width=~F,fontname=Helvetica,"
                                   (gethash name sizes))
                           (format out "fixedsize=true,fontsize=~D"
                                   (truncate (* 10 (gethash name sizes)))))
                         "")
                     (format out "];~%")))
		 graph)
      (format out "}~%"))
    (if render?
	(let ((f (regex-replace "\.[a-z]+$" file (format nil "\.~A" format)))
	      (program (case render?
			 (:hierarchical (which "dot"))
			 (:circular     (which "circo"))
			 (:radial       (which "twopi"))
			 (:spring       (or (which "fdp") (which "neato")))
			 (otherwise     (or (which "fdp") (which "dot"))))))
	  (if program
	      (multiple-value-bind (output error-output exit-status)
		  (trivial-shell:shell-command
		   (format nil "~A -T~A -o ~A ~A" program format f file))
		(unless (= 0 exit-status)
		  (error "~A exited with status ~A: ~A ~A~%" program exit-status output error-output)))
	      (format t "Unable to create PNG of graph ~A.  Graphviz not in your path.~%" graph))
	  f)
	file)))

(defmethod visualize ((graph typed-graph) &key (file "/var/tmp/graph.dot") render?
                                            colors sizes (format "svg"))
  "Save a dot file of this graph. Render can be one of (:heirarchical
:circular :radial :spring), which will render the graph using the appropriate
Graphviz tool."
  (let ((memory (make-hash-table :test 'equalp))
	(connector (if (directed? graph) "->" "--")))
    (with-open-file (out file
			 :direction :output
			 :if-exists :supersede
			 :if-does-not-exist :create)
      (format out
              "~A graphutils~%{~%  splines=true;~%  node [ color = black, "
	      (if (directed? graph) "digraph" "graph"))
      (format out "fillcolor = white, style = filled ];~%")
      (map-nodes (lambda (name id)
                   (let ((neighbors (if (directed? graph)
                                        (outbound-neighbors graph id)
                                        (neighbors graph id))))
                     (dolist (n neighbors)
                       (let ((edge-type (car n)))
                         (unless (if (directed? graph)
                                     (gethash (list id n) memory)
                                     (or (gethash (list id n) memory)
                                         (gethash (list n id) memory)))
                           (setf (gethash (list id n) memory) t)
                           (when (not (directed? graph))
                             (setf (gethash (list n id) memory) t))
                           (format out
                                   "  \"~A\" ~A \"~A\" [w=~A,label=\"~A\"];~%"
                                   name
                                   connector
                                   (gethash (cdr n) (ids graph))
                                   (saref (gethash edge-type (matrix graph)) id (cdr n))
                                   edge-type))))
                     (format out "  \"~A\" [fillcolor=\"~A\""
                             name
                             (if (hash-table-p colors)
                                 (gethash name colors)
                                 "#ffff00"))
                     (if (hash-table-p sizes)
                         (progn
                           (format out ",shape=box,width=~F,fontname=Helvetica,"
                                   (gethash name sizes))
                           (format out "fixedsize=true,fontsize=~D"
                                   (truncate (* 10 (gethash name sizes)))))
                         "")
                     (format out "];~%")))
		 graph)
      (format out "}~%"))
    (if render?
	(let ((f (regex-replace "\.[a-z]+$" file (format nil "\.~A" format)))
	      (program (case render?
			 (:hierarchical (which "dot"))
			 (:circular     (which "circo"))
			 (:radial       (which "twopi"))
			 (:spring       (or (which "fdp") (which "neato")))
			 (otherwise     (or (which "fdp") (which "dot"))))))
	  (if program
	      (multiple-value-bind (output error-output exit-status)
		  (trivial-shell:shell-command
		   (format nil "~A -T~A -o ~A ~A" program format f file))
		(unless (= 0 exit-status)
		  (error "~A exited with status ~A: ~A ~A~%" program exit-status output error-output)))
	      (format t "Unable to create PNG of graph ~A.  Graphviz not in your path.~%" graph))
	  f)
	file)))
