(asdf:defsystem #:cl-directed-graph
  :name "cl-directed-graph"
  :version "20161106"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "Directed graph data structure"
  :serial t
  :components ((:file "package")
               (:file "src"))
  :depends-on (serapeum))
