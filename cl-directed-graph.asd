(asdf:defsystem #:cl-graph
  :name "cl-directed-graph"
  :author "Hunter Chandler <wohonajax@gmail.com>"
  :license "MIT"
  :description "Directed graph data structure"
  :serial t
  :components ((:file "package")
               (:file "src"))
  :depends-on (alexandria anaphora))
