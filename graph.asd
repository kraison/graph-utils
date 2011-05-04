;; ASDF package description for graph              -*- Lisp -*-

(defpackage :graph-system (:use :cl :asdf))
(in-package :graph-system)

(defsystem graph
  :name "graph"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Graph utilities."
  :depends-on (:cl-ppcre)
  :components ((:file "graph-package")
	       (:file "graph-class" :depends-on ("graph-package"))
	       (:file "graph-methods" :depends-on ("graph-class"))))
