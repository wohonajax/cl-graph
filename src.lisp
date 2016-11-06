(in-package #:cl-graph)

(defclass graph ()
  ((vertices :accessor vertices :initarg :vertices)
   (edges    :accessor edges    :initarg :edges)))

(defclass directed-graph (graph) ())

(macrolet ((mem (x y) `(member (cons ,x ,y) (edges graph) :test #'equalp))
           (aif-test (test)
             `(aif ,test (values it t) (values nil nil))))
  (defgeneric adjacent (graph x y)
    (:documentation
     "Tests whether GRAPH contains an edge from X to Y and returns it.")
    (:method ((graph directed-graph) x y)
      (aif-test (mem x y)))
    (:method ((graph graph) x y)
      (aif-test (or (mem x y) (mem y x))))))

(macrolet ((mem (v key)
             `(member ,v (edges graph) :key ,key :test #'equalp)))
  (defgeneric neighbors (graph vertex)
    (:documentation
     "Returns a list of all vertices where there is an edge from VERTEX.")
    (:method ((graph directed-graph) vertex)
      (remove-if-not (lambda (v)
                       (mem v #'car))
                     (vertices graph)))
    (:method ((graph graph) vertex)
      (remove-if-not (lambda (v)
                       (or (mem v #'car) (mem v #'cdr)))
                     (vertices graph)))))

(defun add-vertex (graph vertex)
  "Adds VERTEX to GRAPH."
  (pushnew vertex (vertices graph) :test #'equalp)
  graph)

(defun remove-vertex (graph vertex)
  "Removes VERTEX from GRAPH."
  (setf (vertices graph) (remove vertex (vertices graph) :test #'equalp)
        (edges graph) (remove-if (lambda (x)
                                   (or (equalp (car x) vertex)
                                       (equalp (cdr x) vertex)))
                                 (edges graph)))
  graph)

(defun edge-eql (a b)
  (or (equal a b)
      (equal (cons (cdr a) (car a))
             b)))

(defgeneric add-edge (graph x y)
  (:documentation "Adds an edge from X to Y to GRAPH.")
  (:method ((graph directed-graph) x y)
    (pushnew (cons x y) (edges graph) :test #'equal)
    graph)
  (:method ((graph graph) x y)
    (pushnew (cons x y) (edges graph) :test #'edge-eql)
    graph))

(macrolet ((rem-edge (test)
             `(setf (edges graph)
                    (remove (cons x y) (edges graph) :test ,test))))
  (defgeneric remove-edge (graph x y)
    (:documentation "Removes an edge from X to Y from GRAPH.")
    (:method ((graph directed-graph) x y)
      (rem-edge #'equal)
      graph)
    (:method ((graph graph) x y)
      (rem-edge #'edge-eql)
      graph)))

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

(defun make-graph (&key vertices edges (directed t))
  (make-instance (if directed 'directed-graph 'undirected-graph)
                 :vertices vertices :edges edges))
