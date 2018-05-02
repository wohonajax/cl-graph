(in-package #:cl-directed-graph)

(defclass graph ()
  ((vertices :accessor vertices :initarg :vertices)
   (edges    :accessor edges    :initarg :edges)))

(defmethod print-object ((object graph) stream)
  (print-unreadable-object (object stream)
    (format stream "DIRECTED GRAPH ~:S" (vertices object))))

(defun adjacent (graph x y)
  "Tests whether GRAPH contains an edge from X to Y."
  (if (member y (gethash x (edges graph)) :test #'equal)
      t nil))

(defun neighbors (graph vertex)
  "Returns a list of all vertices in GRAPH where there is an edge from VERTEX."
  (values (gethash vertex (edges graph))))

(defun add-vertex (graph vertex)
  "Adds VERTEX to GRAPH."
  (pushnew vertex (vertices graph) :test #'equal)
  graph)

(defun remove-vertex (graph vertex)
  "Removes VERTEX from GRAPH."
  (setf (vertices graph) (remove vertex (vertices graph) :test #'equal))
  (let ((edges (edges graph)))
    (maphash (lambda (k v)
               (cond ((equal k vertex) (remhash k (edges graph)))
                     ((member vertex v :test #'equal)
                      (setf (gethash k edges) (remove vertex v :test #'equal)))
                     (t)))
             edges))
  graph)

(defun add-edge (graph x y)
  "Adds an edge from X to Y in GRAPH."
  (pushnew y (gethash x (edges graph)) :test #'equal)
  graph)

(defun remove-edge (graph x y)
  "Removes the edge from X to Y in GRAPH."
  (setf (gethash x (edges graph))
        (remove y (gethash x (edges graph)) :test #'equal))
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
  "Creates a new directed graph object. VERTICES should be a list of vertices
unique under EQUAL. EDGES should be a hash table whose keys are objects in
VERTICES and whose values are lists of vertices."
  (when edges
    (maphash (lambda (k v)
               (assert (and (member k vertices :test #'equal)
                            (mapc (lambda (x) (member x vertices :test #'equal))
                                  v))))
             edges))
  (make-instance 'graph :vertices vertices
                 :edges (if edges edges (make-hash-table :test #'equal))))

;;; TRAVERSAL

(defun map-vertices (function graph)
  "Returns a list of the results of calling FUNCTION on each vertex of GRAPH."
  (mapcar function (vertices graph)))

(defun map-edges (function graph)
  "Returns a list of the results of calling FUNCTION on each edge of GRAPH.
FUNCTION should take 2 arguments: the starting and ending vertices of an edge."
  (let ((result (list)))
    (maphash (lambda (k v)
               (mapc (lambda (x)
                       (push (funcall function k x) result))
                     v))
             (edges graph))
    result))
;;; BUG: ordered mapping causes an infinite loop on cycles
(defmacro traverse (&body body)
  `(progn (cond ((member vertex seen :test #'equal))
                (t (push vertex seen)
                   (push (funcall function vertex) result)))
          (dolist (x (neighbors graph vertex))
            ,@body)))

(defun map-depth-first (function graph)
  "Returns a list of the results of calling FUNCTION on each vertex of GRAPH
in depth-first traversal order."
  (lret ((seen (list))
         (result (list)))
    (labels ((trav (vertex)
               (traverse (trav x))))
      (map-vertices #'trav graph))))

(defun map-breadth-first (function graph)
  "Returns a list of the results of calling FUNCTION on each vertex of GRAPH
in breadth-first traversal order."
  (lret ((seen (list))
         (result (list)))
    (flet ((trav (vertex)
             (traverse (push x seen)
                       (push (funcall function x) result))))
      (map-vertices #'trav graph))))
