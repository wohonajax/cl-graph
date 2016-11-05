(in-package #:cl-graph)

(defclass graph ()
  ((vertices :accessor vertices :initarg :vertices)
   (edges    :accessor edges    :initarg :edges)))

(defclass directed-graph (graph) ())
