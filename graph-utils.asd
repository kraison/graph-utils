;; ASDF package description for graph              -*- Lisp -*-

(defpackage :graph-utils-system (:use :cl :asdf))
(in-package :graph-utils-system)

(defsystem graph-utils
  :name "graph-utils"
  :maintainer "Kevin Raison"
  :author "Kevin Raison <last name @ chatsubo dot net>"
  :version "0.1"
  :description "Graph utilities."
  :depends-on (:cl-ppcre :dso-lex :yacc)
  :components ((:file "graph-package")
	       (:file "graph-class" :depends-on ("graph-package"))
	       (:file "graph-methods" :depends-on ("graph-class"))
	       (:file "parsers" :depends-on ("graph-methods"))))
