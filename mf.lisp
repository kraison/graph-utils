(ql:quickload "graph-utils")
(use-package :graph-utils)
(let ((graph (parse-pajek "data/flow.net")))
  (compute-maximum-flow graph 0 5))