(in-package #:cl-user)

(defpackage #:cl-directed-graph
  (:use #:cl)
  (:import-from #:serapeum #:lret)
  (:export #:make-graph
           #:adjacent
           #:neighbors
           #:add-vertex
           #:remove-vertex
           #:add-edge
           #:remove-edge
           #:graph
           ;; traversal functions
           #:map-vertices
           #:map-edges))
