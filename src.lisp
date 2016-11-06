(in-package #:cl-directed-graph)

(defclass graph ()
  ((vertices :accessor vertices :initarg :vertices)
   (edges    :accessor edges    :initarg :edges)))

(defun adjacent (graph x y)
  "Tests whether GRAPH contains an edge from X to Y."
  (aif (member (cons x y) (edges graph) :test #'equal)
       it nil))

(defun neighbors (graph vertex)
  "Returns a list of all vertices in GRAPH where there is an edge from VERTEX."
  (loop :for x :in (vertices graph)
     :when (equalp vertex (rassoc-value (edges graph) x :test #'equal))
     :collect x))

(defun add-vertex (graph vertex)
  "Adds VERTEX to GRAPH."
  (pushnew vertex (vertices graph) :test #'equal)
  graph)

(defun remove-vertex (graph vertex)
  "Removes VERTEX from GRAPH."
  (setf (vertices graph) (remove vertex (vertices graph) :test #'equal)
        (edges graph) (remove-if (lambda (x)
                                   (or (equalp (car x) vertex)
                                       (equalp (cdr x) vertex)))
                                 (edges graph)))
  graph)

(defun edge-eql (a b)
  (or (equal a b)
      (equal (cons (cdr a) (car a))
             b)))

(defun add-edge (graph x y)
  "Adds an edge from X to Y in GRAPH."
  (pushnew (cons x y) (edges graph) :test #'equal)
  graph)

(defun rem-edge (graph x y)
  "Removes the edge from X to Y in GRAPH."
  (setf (edges graph) (remove (cons x y) (edges graph) :test #'equal))
  graph)
#| TODO
(macrolet ((get-vertex-value ()
             '(value-of-vertex (assoc-value vertex (vertices graph)))))
  
  (defun vertex-value (graph vertex)
    "Returns the value associated with VERTEX in GRAPH."
    (get-vertex-value))

  (defun (setf vertex-value) (graph vertex value)
    (setf (get-vertex-value) value)))

(macrolet ((get-edge-value ()
             '(value-of-edge (assoc-value (edges graph) (cons x y)
                              :test #'equal))))
  
  (defun edge-value (graph x y)
    "Returns the value associated with the edge from X to Y in GRAPH."
    (get-edge-value))

  (defun (setf edge-value) (graph x y value)
    (setf (get-edge-value) value)))
|#
(defun make-graph (&key vertices edges)
  (make-instance 'graph :vertices vertices :edges edges))
