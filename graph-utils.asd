;; ASDF package description for graph              -*- Lisp -*-

(defpackage :graph-utils-system (:use :cl :asdf))
(in-package :graph-utils-system)

(defsystem graph-utils
  :name "graph-utils"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Graph utilities."
  :depends-on (:cl-ppcre
	       :dso-lex
	       :yacc
	       :trivial-shell
	       :parse-number
	       :simple-rgb)
  :components ((:file "graph-package")
               (:file "queue" :depends-on ("graph-package"))
               (:file "utilities" :depends-on ("queue"))
               (:file "node" :depends-on ("utilities"))
               ;;(:file "edge" :depends-on ("node"))
	       (:file "graph-class" :depends-on ("node"))
	       (:file "graph-methods" :depends-on ("queue" "graph-class"))
               (:file "maximum-flow" :depends-on ("graph-methods"))
               (:file "bipartite" :depends-on ("maximum-flow"))
	       (:file "graph-generation" :depends-on ("graph-methods"))
	       (:file "graph-visualization" :depends-on ("graph-methods"))
	       (:file "parsers" :depends-on ("graph-methods"))))
