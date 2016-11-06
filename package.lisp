(in-package #:cl-user)

(defpackage #:cl-directed-graph
  (:use #:cl)
  (:import-from #:alexandria #:assoc-value #:rassoc-value)
  (:import-from #:anaphora #:it #:aif)
  (:export #:make-graph
           #:adjacent
           #:neighbors
           #:add-vertex
           #:remove-vertex
           #:add-edge
           #:remove-edge
           #:graph))
