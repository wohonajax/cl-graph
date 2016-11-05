(in-package #:cl-user)

(defpackage #:cl-graph
  (:use #:cl)
  (:import-from #:alexandria #:assoc-value)
  (:import-from #:anaphora #:it #:aif)
  (:export #:make-graph
           #:adjacent
           #:neighbors
           #:add-vertex
           #:remove-vertex
           #:add-edge
           #:remove-edge
           #:vertex-value
           #:edge-value
           #:graph
           #:directed-graph
           #:vertex
           #:directed-vertex))
